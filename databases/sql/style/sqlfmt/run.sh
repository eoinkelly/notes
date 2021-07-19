#!/usr/bin/env bash

# https://github.com/mjibson/sqlfmt

set -x
# set -e

sqlfmt --print-width 80 --tab-width 4 --use-spaces <../example-simple-2.sql > ./example-simple-2.sqlfmt.sql
sqlfmt --print-width 80 --tab-width 4 --use-spaces <../example-simple.sql > ./example-simple.sqlfmt.sql
sqlfmt --print-width 80 --tab-width 4 --use-spaces <../example-with-as.sql > ./example-with-as.sqlfmt.sql
sqlfmt --print-width 80 --tab-width 4 --use-spaces <../example-sql-copy.sql > ./example-sql-copy.sqlfmt.sql
