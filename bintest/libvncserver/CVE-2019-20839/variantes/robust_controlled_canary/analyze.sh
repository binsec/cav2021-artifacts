#!/usr/bin/env bash

mem="$(readlink -f mem)"
cd ../..
exec ./analyze.sh -sse-robust -sse-memory "$mem" "$@"
