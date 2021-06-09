#!/usr/bin/env bash

set -x
set -e

pg_format -c ./pg_format.conf -o example-simple-pg_format.sql ../example-simple.sql
pg_format -c ./pg_format.conf -o example-with-as-pg_format.sql ../example-with-as.sql
pg_format -c ./pg_format.conf -o example-sql-copy-pg_format.sql ../example-sql-copy.sql
