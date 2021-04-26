#!/bin/sh
params="$(cat directives)"
cd ../..
exec ./analyze.sh -sse-directives "$params" "$@"

