#!/usr/bin/env bash

set -e

COMMON_ARGS=("$@" -- -fml-solver z3 -fml-solver-timeout 0 -fml-universal-mode quantifier)

run () {
  file="test_results/$1.csv"
  if [ -f "$file" ]; then
    echo "skipping running tests for $file because it already exists, remove the file to rerun"
  else
    ./run_tests.py  "$@"
  fi
}


run no_robust "${COMMON_ARGS[@]}" -sse-no-robust -sse-ignore-controlled
run exploration_opportunistic_merge "${COMMON_ARGS[@]}" -sse-robust-merge opportunistic -sse-robust-mode exploration
run exploration_no_merge "${COMMON_ARGS[@]}" -sse-robust-merge no -sse-robust-mode exploration
run validation_merge "${COMMON_ARGS[@]}" -sse-robust-mode validation -sse-robust-merge yes
run validation_no_merge "${COMMON_ARGS[@]}" -sse-robust-mode validation -sse-robust-merge no
run bmc "${COMMON_ARGS[@]}" -sse-robust-mode validation -sse-yolo -sse-robust-merge yes
run bmc_no_robust "${COMMON_ARGS[@]}" -sse-no-robust -sse-ignore-controlled -sse-yolo -sse-robust-merge yes
