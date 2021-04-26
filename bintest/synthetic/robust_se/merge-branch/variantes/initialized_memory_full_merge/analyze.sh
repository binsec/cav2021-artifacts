#!/bin/sh

mem="$(readlink -f mem)"
cd ../..
exec ./analyze.sh -sse-robust-mode validation -sse-robust-merge yes -sse-memory "$mem" "$@"
