#!/bin/sh
DIR="$(dirname "$(readlink -f "$0")")"
parallel -i sh -c 'cd $(dirname {}); test_one.sh '"$*" -- $(find $DIR -name expect)
