#!/usr/bin/env bash

set -x
set -e

sqlfluff fix --fixed-suffix "-fixed-by-sqlfluff" --dialect postgres ../example-with-as.sql
