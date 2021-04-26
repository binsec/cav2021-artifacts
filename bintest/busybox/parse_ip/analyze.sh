#!/bin/sh
exec "${BINSEC:-binsec}" sse -isa x86 -entrypoint 0x8048100 -sse-explore 0x804813b -sse-no-explore 0x8048131 -sse-depth 30000 -x86-protected-mode -fml-universal-mode taint -fml-solver boolector -fml-optim-all -sse-robust -sse-memory mem "$@" a.out
