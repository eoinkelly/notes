# The art of PostgreSQL

* latest SQL standard is 2016
* every SQL statement embeds some business logic

### pgloader

```bash
$ brew install pgloader
$ createdb chinook
$ pgloader https://github.com/lerocha/chinook-database/raw/master/ChinookDatabase/DataSources/Chinook_Sqlite_AutoIncrementPKs.sqlite pgsql:///chinook
```

## psql

psql Tricks


```
-- show all variable values
\set

-- show the SQL used by built-in commands like \l, \d etc.
\set ECHO_HIDDEN 'on'


\edit -- edit the last sql query in $EDITOR
```

