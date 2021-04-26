#!/bin/sh

BINSEC=${BINSEC:-binsec}

exec "${BINSEC}" -config config.ini "$@"
