#!/usr/bin/env bash

exec "${BINSEC:-binsec}" \
    -entrypoint 0x080485ac \
    -sse-explore 0x08048649 -sse-no-explore 0x08048650 \
    -isa x86 \
    -sse-memory mem -sse-jump-enum 20 \
    -sse-load-ro-sections \
    -fml-solver boolector \
    -sse-depth 10000 \
    "$@" a.out     sse 
