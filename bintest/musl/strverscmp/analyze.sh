#!/usr/bin/env bash
exec "${BINSEC:-binsec}" sse -isa x86 -entrypoint 0x8048100 -sse-explore 0x8048150 -sse-no-explore 0x804812e -sse-depth 10000 -sse-memory mem -fml-optim-all -fml-universal-mode taint -fml-optim-ai -sse-robust "$@" a.out
