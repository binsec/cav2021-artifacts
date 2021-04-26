#!/usr/bin/env python3
import math
import argparse
from typing import Any, Dict, List, Union

import numpy as np
import pandas as pd

pd.set_option("display.max_columns", None)
pd.set_option("display.max_colwidth", None)
pd.set_option('display.width', 160)


parser = argparse.ArgumentParser()
parser.add_argument('--details', action="store_true")
args = parser.parse_args()

interesting_columns = [
        "status",
        "solver/unknown",
        # "sse/merged_paths",
        "avg_solver_time"]
# "avg_simplification_time",
# "unsimplified_universal_ratio",
# "avg_merge_time"]


def open_csv(name):
    x = pd.read_csv(name)
    return x.set_index("test")


tests = open_csv("tests.csv")


def open_results(filename, name):
    x = open_csv(filename)
    x = pd.merge(tests, x, on="test")
    x["avg_solver_time"] = x["solver/time"] / x["solver/total"]
    x["avg_simplification_time"] = x["formula_transformation/time"] / x["formula_transformation/calls"]
    x["avg_merge_time"] = x["sse/merging_time"] / x["sse/merged_paths"]
    x["unsimplified_universal_ratio"] = x["formula_transformation/universal_quantifier_remains"] / x["formula_transformation/universal_calls"]
    x["merge_completion_ratio"] = x["sse/merging_completed"] / x["sse/merging_started"]
    x["unknown_ratio"] = x["solver/unknown"] / x["solver/total"]
    x.display_name = name
    return x


no_robust = open_results("test_results/no_robust.csv", "SE")


robust_tests = tests.loc[lambda line: line["robust"]].index


def is_ok(line):
    return line["status"] == "SUCCESS"


def is_nok(line):
    return line["status"] != "SUCCESS"


def is_long(line):
    return line["time"] > 1


def is_real(line):
    return ~line.index.str.contains("synthetic/")


def is_reach(line):
    return line["reach"]


def is_unreach(line):
    return ~line["reach"]


def geometric_mean(series):
    return np.exp(np.log(series).sum()/series.count())


def is_ok_or_timeout(line):
    return (line["status"] == "SUCCESS") | (line["status"] == "TIMEOUT")


def is_not_timeout(line):
    """ or invalid or...."""
    return (line["status"] == "SUCCESS") | (line["status"] == "FAILED")


def compute_slowdown(before, after, column_name: Union[str, List[str]], timeouts: bool):
    tests = set(before.index) & set(after.index) & set(robust_tests)
    before_ok = before.loc[tests][is_long][is_ok_or_timeout if timeouts else is_ok].index
    after_ok = before.loc[tests][is_long][is_ok_or_timeout if timeouts else is_ok].index

    ok = before_ok & after_ok

    slow = after.loc[ok][column_name]/before.loc[ok][column_name]

    return slow


def remap_result(df, name: str, before: str, after: str):
    actual = df.loc[name, "status"]
    if before != "TIMEOUT" and actual == "TIMEOUT":
        return
    if actual != before:
        raise ValueError(f"wanted to map {before} to {after} as result of {name} but got {actual}")
    df.loc[name, "status"] = after


exploration_opportunistic_merge = open_results("test_results/exploration_opportunistic_merge.csv", "RSE∀+")
# actually it's OOM
remap_result(exploration_opportunistic_merge, "libvncserver/CVE-2019-20839/variantes/robust_controlled_canary", "INVALID", "Resource exhaustion")
exploration_no_merge = open_results("test_results/exploration_no_merge.csv", "RSE∀")
validation_no_merge = open_results("test_results/validation_no_merge.csv", "RSE")
validation_merge = open_results("test_results/validation_merge.csv", "RSE+")
rbmc = open_results("test_results/bmc.csv", "RBMC")
bmc = open_results("test_results/bmc_no_robust.csv", "BMC")

# false positives


def categorize(line):
    reached = not math.isnan(line["trace_length"])
    complete = line["sse/complete"] or reached
    status = line["status"]
    if complete:
        if is_ok(line):
            return "Correct"
        if status == "FAILED":
            if reached:
                return "False positive"
            return "False negative"
    if line["status"] == "TIMEOUT":
        return "Resource exhaustion"
    if line["status"] in ("SUCCESS", "FAILED"):
        # this incomplete anyway
        return "Inconclusive"
    # for INVALID
    return line["status"]


def no_nan(x: float) -> int:
    if math.isnan(x):
        return 0
    y = int(x)
    assert x == y
    return y


def to_category_table(df, separate_synthetic=False):
    cat = df.apply(categorize, axis=1)
    if separate_synthetic:
        by = cat.groupby(cat.index.str.contains("synthetic/"))
        cat = by.rename(index={False: "Real", True: "Synthetic"}).swaplevel()
    return cat.value_counts()


def to_comparison(d: Dict[str, Any]):
    to_merge = [to_category_table(df).to_frame(name=name) for (name, df) in d.items()]
    merge = to_merge[0]
    for i in to_merge[1:]:
        merge = pd.merge(merge, i, left_index=True, right_index=True, how="outer")
    return merge.applymap(no_nan).sort_index()


def add_total(df):
    s = df.swaplevel().sum(level=1).assign(type="Total").set_index("type", append=True)
    return pd.concat([df, s]).sort_index()


print("### Correctness comparison of normal SE and best robust SE variant")
pivot = to_comparison({"SE": no_robust.loc[robust_tests], "RSE+": validation_merge.loc[robust_tests]})
print(pivot)

if args.details:
    print("###Failures with SE")
    print(no_robust.loc[is_nok][interesting_columns])

    print("###Failures with RSE+")
    print(validation_merge.loc[is_nok][interesting_columns])

contenders = [
        no_robust,
        bmc,
        exploration_no_merge,
        exploration_opportunistic_merge,
        validation_no_merge,
        validation_merge,
        rbmc
        ]

print("### Correctness comparison of all methods")
pivot = to_comparison({x.display_name: x.loc[robust_tests] for x in contenders})
print(pivot)


def analyze_slowdown(before, after, timeouts: bool):
    slowdown = compute_slowdown(before=before, after=after, column_name=["time", "avg_solver_time", "sse/covered_instructions", "sse/instructions", "sse/paths"], timeouts=timeouts)
    gm = geometric_mean(slowdown)
    print("#"*10)
    print(f" slowdown going from {before.display_name} to {after.display_name} (with timeouts: {timeouts}) -> geometric_mean  (2 means enabling is slower): \n{gm}")
    print("#"+"~"*19)
    print(slowdown.describe())
    if args.details:
        print("#"+"~"*19)
        print("# Details of the contribution of all tests")
        print(slowdown)


slowdowns = [
    (no_robust, validation_no_merge),
    (validation_no_merge, validation_merge),
    (no_robust, exploration_no_merge),
    (exploration_no_merge, exploration_opportunistic_merge),
    (no_robust, exploration_opportunistic_merge),
    (no_robust, validation_merge),
    (validation_no_merge, exploration_no_merge),
    (validation_merge, exploration_opportunistic_merge),
]
for (before, after) in slowdowns:
    analyze_slowdown(before, after, True)
    analyze_slowdown(before, after, False)

print("###total time")
for i in contenders:
    print(i.display_name, i["time"].loc[robust_tests].sum(), sep="\t")
print("###total time no timeout")
for i in contenders:
    print(i.display_name, i.loc[robust_tests][is_not_timeout]["time"].sum(), sep="\t")
