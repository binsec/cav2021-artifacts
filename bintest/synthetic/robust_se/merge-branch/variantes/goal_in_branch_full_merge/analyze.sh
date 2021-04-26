#!/bin/sh

cd ../..
exec ./analyze.sh -sse-robust-mode validation -sse-directives '0x804925c reach; 0x804927a cut' "$@"
