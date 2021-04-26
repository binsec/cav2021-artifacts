#!/usr/bin/env bash
exec  "${BINSEC:-binsec}" -config config.ini "$@"
