#!/bin/sh

cd ../..
exec ./analyze.sh -sse-robust-mode validation -sse-robust-merge yes "$@"
