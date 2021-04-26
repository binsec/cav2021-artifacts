#!/usr/bin/env bash

BIN="very_success.exe"
MEMDESC="memory_desc.txt"

exec "${BINSEC:-binsec}" -sse \
  -isa x86 \
  -entrypoint 0x0040104c \
  -sse-no-explore 0x004010d7 \
  -sse-explore 0x004010d5 \
  -sse-depth 10000 \
  -sse-memory ${MEMDESC} \
  -fml-optim-all -fml-optim-ai \
  -fml-solver-timeout 0 \
  -fml-universal-mode taint \
  -sse-robust \
  "$@" ${BIN}
