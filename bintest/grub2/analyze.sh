#!/bin/sh
exec "${BINSEC:-binsec}" -config config.ini "$@"
