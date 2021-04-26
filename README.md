Our experimental evaluation of BINSEC/RSE can be reproduced in the `bintest`
folder. See the README there for full details on how to reproduce experiments
and use binsec on your own. Quick summary:
```
cd bintest
nix-shell
# a shell opens, in this shell, run
./run_all_tests.sh --timeout 120
./dump.py | less
```
`nix-shell` comes for the nix package manager, see `bintest/Readme.md` or
nixos.org to install it.
./test_results contains our raw results in case you
don't want to rerun everything.

Folders ./binsec ./libase and ./unisim contain the source code from which the
tool is built. They are subject to their individual license.

./binsec contains the source code for BINSEC/RSE the symbolic execution engine itself. However, folder ./bintest is still the folder where you will build it.
./unisim is a dependency of binsec used to decode armv7 executables.
./libase contains ocaml code for the abstract interpretation domains we use (intervals and congruence) to simplify SMT formulas.

Important parts of the code:
The code which drives SE is in binsec/src/sse/sse.ml
* path merging for RSE+ in function do_directive, match branch for reach
* path merging for RSE∀+ in function ite and go
The code which adds a universal quantifier is in binsec/src/formula/formula_transformation.ml in function to_universal

We also provide ways to reproduce a secondary claim: in table 1, we claim that
BINSEC/RSE has no false positive with the canary whereas BINSEC vanilla and angr has one.
BINSEC/RSE: `cd bintest; nix-shell --run "cd synthetic/robust_se/stack_protector/variantes/robust_uncontrolled_canary; ./analyze.sh -fml-universal-mode quantifier -fml-solver z3 -sse-robust-merge yes -sse-robust-mode validation"` says Goal unreachable
BINSEC: `cd bintest; nix-shell --run "cd synthetic/robust_se/stack_protector/variantes/normal; ./analyze.sh"` finds a model.
angr: `cd bintest/synthetic/robust_se/stack_protector/angr; nix-shell --run ./run.py` reaches the target.
