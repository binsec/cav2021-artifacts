#!/usr/bin/env bash
exec "${BINSEC:-binsec}" sse -isa x86 -entrypoint 0x080485a9 -sse-explore 0x80485e4 -sse-no-explore 0x8048607 -sse-depth 10000  "$@" a.out
