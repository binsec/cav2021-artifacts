Our source, simplified to the extreme, takes a controlled input `input`
and an uncontrolled password `password` in global variables, compares them,
and returns the result.
`foo.c`:
```
volatile int input = 0;
volatile int password = 42;

int main() {
  return input == password;
}
```
`volatile` is there to prevent the compiler for propagating constants.
We use global variables for simplicity. If you want to use arguments to a function,
you have to know the calling convention of the function for initialization.

First we must compile this test program to analyze with binsec. Remember that it must be a 32bits fully static executable. If you are not familiar with cross compilation, this can be challenging. We will use `nix` to solve this problem for us.

In a new folder `foo` inside this `bintest` folder where you placed `foo.c`, create `default.nix`:
```
let
  # use vanilla nixpkgs, at a pinned version for reproducibility
  sources = import ../../binsec/nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
  # use 32bit linux static musl version
  pkgs = nixpkgs.pkgsi686Linux.pkgsStatic;
in
  pkgs.mkShell {
    # let's keep the assembly simple
    hardeningDisable = [ "all" ];
  }
```
run `nix-shell default.nix` and inside the shell compile with `$CC`:
`$CC foo.c -g -o foo`.
Check that the executable is really static, with no ELF interpreter:
```
$  file foo
foo: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), statically linked, with debug_info, not stripped
```

Now we need to provide the necessary options to binsec. We need an address for
the goals, and the adresses of `input` and `password` for initialisation. In
this simplistic example, no data is read from .data and .rodata sections, so we
don't need to initialise those.

```
$  objdump --syms foo | grep input
0804c060 g     O .bss	00000004 input
$  objdump --syms foo | grep password
0804c010 g     O .data	00000004 password
$  objdump -d foo | grep -A10 '<main>'
080491f5 <main>:
 80491f5:	55                   	push   %ebp
 80491f6:	89 e5                	mov    %esp,%ebp
 80491f8:	8b 15 60 c0 04 08    	mov    0x804c060,%edx
 80491fe:	a1 10 c0 04 08       	mov    0x804c010,%eax
 8049203:	39 c2                	cmp    %eax,%edx
 8049205:	0f 94 c0             	sete   %al
 8049208:	0f b6 c0             	movzbl %al,%eax
 804920b:	5d                   	pop    %ebp
 804920c:	c3                   	ret
 ```
 This leads us to this config:
 `config.ini`
 ```
[kernel]

isa = x86
file = foo
entrypoint = main

[sse]
enabled = true
robust = true
# RSE+
robust-mode = validation
robust-merge = yes
depth = 1000
memory = mem
# address of the `ret` at the end of main
# @[esp<32>, 4] is the return value of a x86 function returning int
directives = 0x0804920c reach if @[esp<32>, 4]<>0<32>; 0x0804920c cut

[fml]
solver = z3
optim-all = true
universal-mode = quantifier
```
`mem`
```
# or any value that makes esp not overlap with .text
esp<32> := [ 0xfff00000, 0xffffffff ]u;
controlled input<32>;
@[0x0804c010, 4] := password<32>;
@[0x0804c060, 4] := input<32>;
```
It is better to first try binsec in non-robust mode, to catch missing initialisation of memory:
```
$  binsec -config config.ini -sse-no-robust -sse-ignore-controlled
Registering domain assert false with id 1
Registering domain Term_domain with id 2
[sse:result] Directive :: reached address 0x0804920c(main+0x17) with (@[esp<32>,4]
                                                                    <> 0<32>) (0 to go)
[sse:result] Model @ 0x0804920c(main+0x17)
             --- Model ---
             # Variables
             bs_unknown1_for_esp_32__4 : {0xfff00000; 32}
             
             
             # Memory
             0xfff00000 : 0xff 
             default    : 0x00
```
The event "the attacker guessed the password" is thus reachable. But z3 tells us it's only the case if the full initial memory is 0 except on the first byte of the stack, which must be 0xfff00000.

Notice how command line arguments prevail on options in config.ini.

Imagine binsec had told us (excerpt of musl/strptime)
```
[sse:result] Model @ 0x08048161(main+0x61)
             --- Model ---
             # Variables
             [...]
             
             
             # Memory
             ; section .rodata
             0x08049c3b : 0x00 
```
it means that the solver took the liberty to assume some memory in .rodata was 0.
Inspect this address with your favorite tool (ghidra, ida or radare2), it happens
to be a jump table for a switch statement. Initilize it explicitly:
```
@[0x08049c2c, 384] from_file;
```
Initialization is not done automatically because large .rodata/.data sections
choke solvers.


Coming back to `foo.c` let's run SE in robust mode:

```
$  binsec -config config.ini                                      
Registering domain assert false with id 1
Registering domain Term_domain with id 2
[sse:warning] Directive :: reach reached address 0x0804920c(main+0x17) if (@[esp<32>,4]
                                                                    <> 0<32>) with (∀=UNSAT, ∃=UNKNOWN) (still 1 to go)
[sse:result] Directive :: cut @ 0x0804920c(main+0x17)
[sse:result] Goal unreachable.
```
The event "the attacker guessed the password" is thus not robustly reachable.
