# Test suite

### Dependencies

To compile all the dependencies, only the nix package manager and an internet
connection is required. It is possible to install the nix package manager
on most Linux distributions:
```
curl -L https://nixos.org/nix/install | sh
```
(see https://nixos.org/manual/nix/stable/#chap-installation for options).
If you install the debian package `nix-bin` make sure to add yourself
to group `nix-users`, or run everything as root, otherwise you may encounter
permission issues: 
```
error: getting status of /nix/var/nix/daemon-socket/socket: Permission denied
```
See
https://sources.debian.org/src/nix/2.3.10+dfsg1-1/debian/nix-bin.README.Debian/
for details.
It may be more convenient to use a pre-built NixOS vm like this one:
https://hydra.nixos.org/build/134650511/download/1/nixos-20.09.2538.0cfd08f4881-x86_64-linux.ova
When importing the virtual box appliance, you should probably increase the VM
memory (we run experiments with a cap of 7GB memory) and the number of CPUs if
you can. The username is `demo` and the password is `demo` as well.

### Setup

From the top of the tarball, run
```
cd ./bintest
nix-shell
```

After some compilation, this will yield a shell where all dependencies,
including binsec and z3, are available.
Note that tests cannot be run outside of this shell (a bit like with virtualenv).

### Individual tests

Tests are represented as a folder like `./openssl/base64` containing: an
executable to be analysed (usually `a.out`), a script to run binsec with the
right options (`analyze.sh`) and a file containing the expected output (`expect`).
Usually the options passed to binsec are stored in `config.ini` and the initial
state of memory in `mem`. Notably, what variables are controlled and what variables
are not is defined in `mem`.

If a makefile is present, it should recreate the executable from source. This
is not possible for all executables, for example CTFs do not provide
source code. The only dependency for compilation is the nix package manager, again.

Some tests are available in variants. Each variant has its own `analyze.sh`
script in a subfolder.  For example `./libvncserver` has three variants: a non
robust variant, where a false positive is found, and two robust variant where
the canary is or is not controlled. When the canary is not controlled, binsec
founds nothing in reasonable time, and when it is controlled, an attack is
found. This illustrates the protection provided by the canary.

To list tests, `find . -name expect`. A description is provided as a text file in
most cases. Tests in the synthetic folder were written with the purpose of exercising
specific code constructs, and are best described by the corresponding C 
source code.

Inside the folder of a test, you can run `./analyze.sh` to execute binsec,
and `test_one.sh` to run it silently and compare the result to the expected one.

Keep in mind that constraints submitted to SMT solvers do not necessarily have
single solutions; binsec might find different models than what we mention in
the case studies of the paper. Expectation files use the flexibility of
llvm's FileCheck to accept several answers.

### Running tests

With the nix package manager, run `nix-shell` in the `bintest` folder
to obtain a shell with the dependencies installed.

To run all tests, `./run_all_tests.sh --timeout 120 --memlimit 7000 --nproc 6`
`timeout` is in seconds, `memlimit` is in MiB (max memory for a run),
`nproc` is the amount of parallelism.
All arguments are optional, and default to the values of the paper, but they take
about 11h on a 12 core machines. With a timeout of 120s, you should still get
results that highlight the same tendencies within 4h.

To summarize the results you obtained, run `./dump.py`. Slowdown measurements are computed
there.
This should print a lot of text, including the equivalent of table 4 for the
shorter timeout of 120s we advised above:
                     SE  BMC  RSE∀  RSE∀+  RSE  RSE+  RBMC
Correct              27   21    28     32   34    40    31
False positive       16   14     0      0    0     0     0
Inconclusive          0    0    14     10    6     0     1
Resource exhaustion   3   11     4      4    6     6    14

This table should allow you to verify RQ1, RQ2, and part of RQ3. Performance
aspects (in RQ3 and RQ4) can be checked with the rest of the output. For example,
to confirm the overhead of RSE+ over SE in RQ4, search for "Slowdown going from
SE to RSE+ (with timeouts: True)" in the output. Average, geometric mean and
quartiles are available across different parameters. Keep in mind that if you
ran experiments with a different timeout than 1h, results may differ slightly,
but qualitative conclusions should not differ much.

Our raw test results in csv format are stored in `../test_results`.
To run `./dump.py` on those (which allows you not to run the bench yourself),
copy `../test_results` to `.`.

### Using binsec on your own

To build binsec it is enough to run inside `nix-shell` in this folder: `binsec`
can be run inside this shell (but not outside).
You can also run `nix-build nix/pkgs.nix -A binsec_appimage` in the `binsec`
folder, after some compilation this will create an appimage of binsec in a `result`
folder. **Important: an appimage cannot run on NixOS, which is the case of the
provided vm**. This is quite handy on other linux distros, however.

Some reference-level documentation is provided in `./sse.md`. This can also help
understanding the options we set in `config.ini` in our benchmarks.

A small tutorial for the most simplistic of examples is provided in `./guide.md`.

