#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import math
import os
import resource
import signal
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from configparser import ConfigParser
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple

import prctl
from pandas import DataFrame, read_csv


def warn(*args):
    print("\033[93mWARNING:", *args, '\033[0m', file=sys.stderr)


def contains(txt: Iterable[str], *needles: str) -> Optional[bool]:
    """
    Returns whether this expect file expects the needles
    Returns True if there is a line of the form CHECK: ...needle1 ... needle2...
    Returns False with CHECK-NOT
    returns None otherwise

    txt: content of expect
    """
    for i in txt:
        if all((target in i for target in needles)):
            if "CHECK-NOT:" in i:
                return False
            if "CHECK" in i:
                return True
    return None


def argfor(txt: Iterable[str], option: str) -> Optional[str]:
    """
    Returns the argument of option in txt

    txt is meant to be the content of analyze.sh
    """
    for line in txt:
        words = line.strip().split()
        if option in words:
            return words[words.index(option)+1]
    return None


def find_configfile(directory: Path, root: Path) -> Iterable[Path]:
    """
    scans parents dir until config.ini is found, or root is reached
    """
    assert directory.is_dir()
    root = root.resolve()
    while directory != root:
        configfile = directory/"config.ini"
        if configfile.is_file():
            yield configfile
        directory = directory.parent


def load_configfile(cfg: ConfigParser, path: Path) -> None:
    """
    loads binsec's config.ini file into configparser
    """
    with path.open("r") as f:
        txt = f.read()
    cfg.read_string(txt.replace("\\\n", " "), str(path))


def parse_meta(txt: Iterable[str]) -> Dict[str, Any]:
    """
    parses meta
    """
    for line in txt:
        if line.startswith("META:"):
            json_blob = line[len("META:"):]
            return json.loads(json_blob)
    return {}


class TestStatus(Enum):
    SUCCESS = "\033[92mSUCCESS\033[0m"
    FAILED = "\033[91mFAILED\033[0m"
    TIMEOUT = "\033[93mTIMEOUT\033[0m"
    INVALID = "\033[94mINVALID\033[0m"


@dataclass
class TestResult:
    result: TestStatus
    time: float
    trace_length: Optional[int]
    timeout: int
    stats: Dict[str, Dict[str, Any]]

    def __str__(self) -> str:
        return f"{self.result.value} ({self.trace_length} instrs in {self.time} s)"


def suicide():
    os.setpgrp()
    prctl.set_pdeathsig(signal.SIGINT)


class Test:
    name: str
    directory: Path
    expect: Path
    runscript: Path
    executable: Path
    reach: bool
    robust: bool
    ignore: bool
    arch: str

    def __init__(self, expect: Path, root: Path):
        self.directory = expect.parent
        self.name = str(self.directory)
        self.directory = self.directory.absolute()
        self.directory.resolve()
        self.expect = self.directory / expect.name
        del expect
        self.runscript = self.directory / "analyze.sh"
        assert self.runscript.is_file()
        with self.expect.open("r") as f:
            txt = f.read().splitlines()
        meta = parse_meta(txt)
        model = contains(txt, "[sse:result] Model")
        if model is True:
            self.reach = True
        elif model is False:
            self.reach = False
        else:
            model = contains(txt, "sse:result", "unreachable")
            if model is True:
                self.reach = False
            elif model is False:
                self.reach = True
            else:
                model = contains(txt, "sse:warning", "Halting ...")
                if model is True:
                    self.reach = False
                elif model is False:
                    self.reach = True
                elif "reach" in meta:
                    self.reach = meta["reach"]
                else:
                    raise ValueError(f"{self.expect} does not indicate expected reachability")
        self.robust = False
        with self.runscript.open("r") as f:
            runscript = list(f)
        x = argfor(runscript, "-config")
        configfiles: List[Path] = []
        if x is not None:
            configfiles.append(self.runscript.parent / x)
        configfiles += list(find_configfile(self.directory, root))
        config = ConfigParser()
        for configfile in configfiles:
            load_configfile(config, configfile)
            break
        arch = argfor(runscript, "-isa")
        if arch is not None:
            self.arch = arch
        if not hasattr(self, "arch"):
            if "kernel" not in config:
                if "arch" in meta:
                    self.arch = meta["arch"]
                else:
                    raise FileNotFoundError(f"no config found for test {self.name} among {configfiles}")
            else:
                self.arch = config["kernel"]["isa"]
        if "sse" in config:
            self.robust = config["sse"].getboolean("robust", fallback=self.robust)
        if argfor(runscript, "-sse-robust") is not None:
            self.robust = True
        if argfor(runscript, "-sse-no-robust") is not None:
            self.robust = False
        for arg in meta:
            v = meta[arg]
            if hasattr(self, arg):
                old = getattr(self, arg)
                if old != v:
                    warn(f"overriding {arg}: {getattr(self, arg)} -> {meta[arg]}")
            setattr(self, arg, v)
        self.ignore = getattr(self, "ignore", False)

    def __repr__(self) -> str:
        return f"Test({self.directory}, reach={self.reach}, robust={self.robust}, arch={self.arch})"

    @staticmethod
    def find(root: Path) -> Iterable[Test]:
        for entry in root.rglob("expect"):
            assert entry.is_file()
            yield Test(entry.relative_to(root), root)

    def run(self, timeout: int, args: Iterable[str] = ()) -> TestResult:
        with tempfile.TemporaryDirectory() as d:
            tracefile = Path(d) / "trace"
            statsfile = Path(d) / "stats.json"
            start = time.monotonic()
            process = subprocess.Popen([
                str(self.runscript),
                "-sse-address-trace-file", str(tracefile),
                "-sse-stats-file", str(statsfile)
                ]+list(args), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False, cwd=self.directory, preexec_fn=suicide)
            checker = subprocess.Popen(["FileCheck", self.expect], stdin=process.stdout, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, shell=False, preexec_fn=suicide)
            try:
                returncode = process.wait(timeout=timeout)
                stop = time.monotonic()
                checkreturncode = checker.wait(timeout=100)
                result = TestStatus.INVALID if returncode else (TestStatus.FAILED if checkreturncode else TestStatus.SUCCESS)
                if returncode:
                    warn(f"binsec exited with code {returncode}")

            except subprocess.TimeoutExpired:
                stop = time.monotonic()
                result = TestStatus.TIMEOUT
                stats = {}
            finally:
                if process.returncode is None:
                    warn(f"killing process {process.pid}...")
                    process.send_signal(signal.SIGINT)
                    try:
                        process.wait(100)
                    except subprocess.TimeoutExpired:
                        pass
                if checker.returncode is None:
                    warn(f"killing process {checker.pid}...")
                    checker.send_signal(signal.SIGINT)
                    try:
                        checker.wait(100)
                    except subprocess.TimeoutExpired:
                        pass
                try:
                    os.kill(-process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                try:
                    os.kill(-checker.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass

            try:
                with statsfile.open("r") as f:
                    try:
                        stats = json.load(f)
                    except json.JSONDecodeError as e:
                        f.seek(0)
                        content = f.read()
                        warn(f"{statsfile} for test {self.name} contains malformed json: {e}: {content}")
                        stats = {}
            except OSError:
                warn(f"binsec did not create stats file {statsfile}")
                raise

            trace_length: Optional[int]
            try:
                with tracefile.open("r") as f:
                    trace_length = 0
                    for _ in f:
                        trace_length += 1
            except OSError:
                trace_length = None

            if result == TestStatus.SUCCESS and (trace_length is not None) != (self.reach):
                warn(f"test {self} resulted in successful run with trace_length {trace_length!r}")

            return TestResult(result=result, time=stop-start, trace_length=trace_length, timeout=timeout, stats=stats)


def run_test(test: Test, timeout: int, args=()) -> Tuple[Test, TestResult]:
    return (test, test.run(timeout, args))


def tests_to_df(tests: Iterable[Test]) -> DataFrame:
    return DataFrame.from_records(map(lambda test: (test.name, test.reach,
                                                    test.robust), tests), columns=["test", "reach", "robust"])


def extra_columns(result: TestResult, columns: List[str]) -> List[Any]:
    return [result.stats.get(i.split('/')[0], {}).get(i.split("/")[1], float("nan")) for i in columns]


def results_to_df(results: Iterable[Tuple[Test, TestResult]]) -> DataFrame:
    column_names: Set[str] = set()
    for _, result in results:
        for i, j in result.stats.items():
            for k in j:
                column_names.add(f"{i}/{k}")
    columns = list(column_names)
    columns.sort()

    return DataFrame.from_records(map(lambda x:
                                      [x[0].name, x[1].result.name, x[1].time, x[1].trace_length, x[1].timeout] + extra_columns(x[1], columns),
                                      results), columns=["test", "status", "time", "trace_length", "timeout"]+columns)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='run the benchmark')
    parser.add_argument('output', help='output file to write data to, without .csv suffix')
    parser.add_argument('--timeout', type=int, default=60*60, help='individual timeout in second')
    ncpu = os.cpu_count() or 1
    parser.add_argument('--nproc', default=math.ceil(ncpu/2), type=int, help='number of parallel tests, default half number of cpus')
    parser.add_argument('--memlimit', default=7*1024, type=int, help='maximum memory per test, in MiB')
    parser.add_argument('extra', metavar="ARGS", nargs='*', help='extra arguments passed to binsec')
    args = parser.parse_intermixed_args()
    print("Running with ", args)
    resource.setrlimit(resource.RLIMIT_AS, (args.memlimit*1024*1024, args.memlimit*1024*1024))
    script = Path(__file__)
    script.resolve()
    root = script.parent
    testsdir = root/"test_results"
    testsdir.mkdir(exist_ok=True)
    outfile = testsdir/(args.output+".csv")
    if outfile.is_file():
        raise ValueError(f"{outfile} exists")
    tests = [t for t in Test.find(root) if not t.ignore]
    testsdf = tests_to_df(tests)
    testsfile = root/"tests.csv"
    if testsfile.is_file():
        df = read_csv(str(testsfile))
        old = set(map(tuple, df.itertuples(index=False)))
        new = set(map(tuple, testsdf.itertuples(index=False)))
        assert old.issubset(new), (old ^ new)
    testsdf.to_csv(str(testsfile), index=False)

    results: List[Tuple[Test, TestResult]] = []
    with ThreadPoolExecutor(max_workers=max(1, args.nproc)) as e:
        futures = list(map(lambda test: e.submit(run_test, test, args.timeout, args.extra), tests))
        for f in as_completed(futures):
            test, result = f.result()
            print(test, str(result))
            results.append((test, result))
    print(len(results), "tests ran")
    resdf = results_to_df(results)
    resdf.to_csv(outfile, index=False)
