#!/usr/bin/env bash

set -x
set -e

curl -L -O https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite

createdb chinook_data

pgloader ./Chinook_Sqlite_AutoIncrementPKs.sqlite postgresql:///chinook_data
