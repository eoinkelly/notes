#!/usr/bin/env bash

set -x
set -e

sql-lint --config ./config.json ../example-simple.sql
