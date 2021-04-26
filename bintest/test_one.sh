#!/usr/bin/env bash
# Usage: launch in a direcotry containing analyze.sh and expect
# check that all the lines in `expect` are in the output of binsec

set -o pipefail

if DLEVEL=0 ./analyze.sh "$@" 2>&1 | FileCheck expect;
then echo -e "$(pwd): \033[32mPASS\033[0m"; exit 0
else echo -e "$(pwd): \033[31mFAIL\033[0m"; exit 1
fi
