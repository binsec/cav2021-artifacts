#!/bin/sh

mem="$(readlink -f mem)"
cd ../..
exec ./analyze.sh -sse-memory "$mem" "$@"
