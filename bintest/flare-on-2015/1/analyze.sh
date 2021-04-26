#!/bin/sh

BINSEC=${BINSEC:-binsec}
BIN="i_am_happy_you_are_to_playing_the_flareon_challenge.exe"
DO_NOT_EXPLORE="0x40107b"
GOAL="0x401063"
MEMDESC="memory_desc.txt"

exec "${BINSEC:-binsec}" -sse \
  -isa x86 \
  -entrypoint 0x40104b \
  -sse-no-explore ${DO_NOT_EXPLORE} \
  -sse-explore ${GOAL} \
  -sse-depth 10000 \
  -fml-universal-mode taint \
  -sse-memory ${MEMDESC} \
  -fml-optim-all \
  -sse-robust \
  "$@" ${BIN}
