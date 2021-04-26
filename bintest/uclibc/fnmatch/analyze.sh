#!/usr/bin/env bash
exec "${BINSEC:-binsec}" sse -isa x86 -entrypoint 0x80480f0 -sse-explore 0x8048120 -sse-no-explore 0x804812a -sse-depth 1000 -fml-universal-mode taint -x86-protected-mode -fml-optim-all -sse-robust -sse-memory mem "$@" a.out
