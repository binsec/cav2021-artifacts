# SSE tutorial

## Usage

Let `a.out` be the executable you want to execute. Create a file `config.ini` as follows:
```
[kernel]

isa = x86
file = a.out
entrypoint = 0x08049050
[sse]

enabled = true
depth = 1000
memory = mem
directives = \
  0x08049075 reach; \
  0x08049067 cut

[fml]

solver = z3
optim-all = true
```

* `isa` is the architecture the binary is supposed to run on.
* `file` is the path the executable
* `entrypoint` is the address of the first instruction
* `enabled` is what tells binsec that you want it to do SSE. Otherwise it does nothing :)
* `depth` is the length of the longest path to consider. If you have `reach`
  directives, they will only be considered if they can be reached in less than
  `depth` instructions. The default is very low, so make sure to specify this option.
* `memory` is a path to a file describing the initial state of memory and
  registers. The exact syntax is described later.
* `directives` is a semicolon separated (trailing semicolon is not supported!)
  list of directives. Directives are described later.
* `solver` is the smt solver to use: `boolector`, `z3`, `yices` or `cvc4`.
* `optim-all` enables all sorts of nice formula simplifications.

Then run `binsec -config config.ini`.
Alternatively all these options can be specified on the command line, usually
in the form `-${section name}-${option name}`:
```
binsec sse -isa x86 -entrypoint 0x08049050 -sse-depth 1000 -sse-memory mem a.out
```
In the following we refer to options by their command line arguments, but each time
you read "use option `-sse-robust`" you can alternatively add `robust = true` in the
`sse` section of `config.ini`.

## Directives
A directive is of the form `0xaddress verb`. The existing directives are:
* `0xdeadbeef reach`: print a model leading to this address
* `0xdeadbeef reach(12)`: print a model leading to this address the 12 first times it is reached.
* `0xdeadbeef reach *`: print models leading to this address until depth is exceeded
* `0xdeadbeef reach if cond`: print models leading to this address with the DBA
  expression `cond` evaluating to true. Evaluation is done before the side
  effet of the instruction is performed.
* `0xdeadbeef enumerate eax<32> (4)`: reach this address until 4 different possible values of `eax` are found and printed
* `0xdeadbeef assume esp<32> & 0x000000ff = 0<32>;` when reaching this address, assume the expression
* `0xdeadbeef cut;` stop exploration when reaching this address
* `0xdeadbeef +` if 0xdeadbeef is a jump, then explore the branch where the
  jump is not taken first. Eventually both branches will be taken, use `cut` if
  you want the other branch not to be taken instead.
* `0xdeadbeef -` if 0xdeadbeef is a jump, then explore the branch where the jump is
taken first.

Without `reach` or `enumerate` directives binsec has nothing to do and thus does not
explore anything.


## Supported executables
Binsec only supports fully static executables of the following 32bit architectures:
- x86
- armv7
- riscv (not well tested)
It will run fine on dynamic executables, but sections like got and plt that are supposedly populated by the dynamic linker will be uninitialized, and calling a dynamic symbol
will jump anywhere. This is likely not what you want.

To obtain fully static executables we advise you to use musl. glibc cannot create fully static executables.

Binsec does not support floating point instructions nor system calls. There is a system
to replace unsupported instructions by stubs, described later. You will likely stub
full libc functions instead of just system calls.

## Initial state specification
By default binsec starts the process with a fully symbolic memory. That is, data sections are not loaded.
It is very probable that you want to populate the memory with some concrete values.

By default, the initial state of memory is read from a file called `memory.txt`. The general syntax is DBA.
DBA syntax is mostly intuitive except for the following quirks:
* Bitvectors must supply their width: `1<32>` is a 32 bit one, and can also we written as `0x00000001`;
* Some operators like `<` must be suffixed by `u` or `s` to denote signed or unsigned comparison.
Example initial state:
```
# initialize a register with a constant value.
DF<1> := 1<1>;
# initialize a register with a non deterministic interval. ]u means unsigned interval.
esp<32> := [ 0xfff00000, 0xffffffff ]u;
# write the byte 42 at address 0x0804c000. 1 is in bytes, 8 in bits.
@[0x0804c000, 1] := 42<8>;
# choice is a symbolic 32-bits bitvector. It is "declared" implicitely when used.
# write the symbolic value choice at address 0x0804c040
@[0x0804c040, 4] := choice<32>;
# load the content of memory at 0x0804c060 for the executable.
@[0x0804c060, 4] from_file;
```

## Robust SE
In the memory initialization file, declare a symbolic variable as controlled:
```
controlled foo<32>;
```
Then add the argument `-sse-robust` to the command line.
In this case it is essential to assign an interval to `esp`, `ebp` and to set `DF`
in the initial memory. All variables not explicitly declared as controlled are
considered uncontrolled.
Initial memory is always uncontrolled. If you want a controlled input in initial memory at, say, 0xdeadbeef, you must write this in `mem`:
```
controlled foo<32>;
@[0xdeadbeef, 4] := foo<32>;
```

By default, robust SE uses taint, a method not presented in the paper which is very incomplete. Pass `-fml-universal-mode quantifier` to use a quantifier (the method described in the paper). This is only supported by some solvers (z3, cvc4, and if there are no arrays boolector, at the time of this writing): pass `-fml-solver z3` to choose.

Robust SE exists in two main flavors: 
* `-sse-robust-mode validation` explores like SE and only checks that the solution
found is robust on `reach` directives (RSE and RSE+ in the paper)
* `-sse-robust-mode exploration` explores only robustly reachable locations and is
incomplete (RSE∀ and RSE∀+)

Path merging is available with `-sse-robust-merge something` where something is
* `no` no path merging
* `yes` full, systematic path merging. Only available in validation model
* `opportunistic` incomplete path merging in universal path pruning, only available
with exploration mode.

Robust SE fails if no variable is declared controlled in the initial state.
To bypass this check, label an otherwise unused variable as controlled.
Conversely, non robust SE fails if a variable is controlled. To bypass
this check, pass `-sse-ignore-controlled`.

Empirically, z3 does not like universally quantified arrays. Pass `-fml-unquantify-memory`
to get rid of them. The transformation will grow the formula quadratically in the 
number of select.

## Dynamic jumps
Each time a dynamic jump is reached, up to `n` values are enumerated.
`n` defaults to 3 and can be set with the option `-sse-jump-enum`

## Useful options

`-sse-smtdir .`: creates a folder `binsec_sse` in the current directory and
stores the smt files given to the solver there. Combine with `-sse-debuglevel
10` to know what part of stdout corresponds to what smt file. Combine to
`-sse-comment` to have comments in the smt files which tell which parts of the
file correspond to which address.
`-sse-comment` has no effect with universally quantified formulas, because smtlib
does not support inline comments.

`-sse-address-trace-file foo` when a model is found, write the current branch
as a list of instruction addresses to file `foo`.

`-sse-visit-until 0xdeadbeef:4` stop exploration of branches which reach
`0xdeadbeef` at least 4 times. Can be specified multiple times to flag several
addresses.

`-fml-solver-timeout 12`, where 12 is in seconds, to avoid timeouts

`-sse-yolo` does bounded model checking instead of symbolic execution.

## Stubs

You can patch what the disassembler "sees" when reading the binary with the option
`-disasm-decode-replacement`. Example in `config.ini`:

```
[disasm]

decode-replacement = \
0x08048623 -> \
0: eax<32> := 0<32>; goto 1 \
1: eax<32>{0, 7} := @[ebx<32>,1]; goto 2 \
2: goto (0x08048625, 0)  \
0x080487dc -> \
0: ecx<32> := 0<32>; goto 1 \
1: ecx<32>{0, 7} := @[ebx<32>,1]; goto 2 \
2: goto (0x080487de, 0)  \
0x080484b9 -> \
0: edx<32> := 0<32>; goto 1 \
1: edx<32>{0, 7} := @[eax<32>,1]; goto 2 \
2: goto (0x080484bb, 0) 
```

For each address, you specify the exact DBA which will be loaded.
For slight modifications, you can obtain the original DBA of the executable
with this command:

```
binsec disasm -isa x86 -entrypoint 0x08048623 a.out
```

The x86 disassembly is printed to stdout and the DBA disassembly is written to
`out.dba`. When doing so you have to patch all the addresses from the form
`08048623` to the form `0x08048623`, because the parser does not accept the
output of the pretty printer. Use the search and replace regex functionality of
your favorite editor. Then, you can patch the DBA to your liking.
