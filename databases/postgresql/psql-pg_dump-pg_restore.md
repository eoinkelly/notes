# PG tools (psql, pg_dump, pg_restore)

# psql

## defaults

- built-in defaults:
    - username: current OS username
    - password: NONE BUILT-IN
    - host: localhost
    - port: 5432
    - database: NONE BUILT-IN

There are 2 ways to tweak defaults:

1. environment variables
    - PGDATABASE
    - PGHOST
    - PGPORT
    - PGUSER
    - PGPASSWORD
2. the `~/.pgpass` file

### passwords

- There is no way to put your password on the command line because this is
  insecure (it gets logged to your shell history)

- psql will default to showing a password prompt if the server needs a password
  (unless you supply `-w` in which case the connection will fail.

## connecting

- The current version of psql defaults to looking for sockets in `/tmp`
- By default homebrew `pg_hba.conf` looks like

    ```
    # TYPE  DATABASE        USER            ADDRESS                 METHOD

    # "local" is for Unix domain socket connections only
    local   all             all                                     trust
    # IPv4 local connections:
    host    all             all             127.0.0.1/32            trust
    # IPv6 local connections:
    host    all             all             ::1/128                 trust
    ```

    so it only allows connections (via TCP and unix sockets) from my machine.

- `trust` auth method
    - postgres assumes that anybody who can get to the machine, should also be
      given full (any user including root) access to the DB
    - Allow the connection unconditionally. This method allows anyone that can
      connect to the PostgreSQL database server to login as any PostgreSQL user
      they wish, without the need for a password or any other authentication.
    - Restrictions made in the `database` and `user` columns still apply.
        - TODO: what does this mean?

### Examples:

```
# connect to default database with default username on default host, port
$ psql

# connect to mydb database with default username, host, port
$ psql mydb
```

### connection info (conninfo) strings

- https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PARAMKEYWORDS

```
postgres://
postgresql://[user[:password]@][netloc][:port][,...][/dbname][?param1=value1&...]
```

## archive formats

Both `pg_dump` and `pg_restore` support a number of archive formats

- custom
    - understood by `pg_dump` and `pg_restore`
    - best choice if going from PG to PG
    - most flexible format
        - allows manual selection and reording of items during restore
    - compressed by default
- directory
    - makes
        - a directory for db,
        - one file for each table and blob being dumped
        - pg_restore readable table of contents file
    - compressed by default
    - supports parallel dumping
- tar
    - use a tar file
    - if untarred is the same as a the `directory` format
    - does not support compression

## pg_dump

```
PGPASSWORD=mypassword pg_dump -Fc --no-acl --no-owner -h localhost -U myuser mydb > kete.pg_dump

# --format=? (-F)
    * select output format

# --format=c (-F)
    * select the custom-format archive
    * suitable for use with pg_restore,
    * compressed by default

--no-owner
  * don't output commands that set the ownership of objects to match the original DB
  * only meaningful for the `text` output format. pg_restore can control this if you use other formats

-h
  * host that DB is on

-U
  * database user

--no-acl
  * prevent dumping of access privileges

```

## pg_restore

```
 pg_restore --verbose --clean --no-acl --no-owner -h localhost -U myuser -d mydb latest.dump


pg_restore can figure out the format of the archive automatically

 --no-acl
  * do not restore access priveleges

--no-owner
  * do not restore the ownership of objects to match the original DB

--clean
  * Clean (drop) DB objects before recreating them
      * might generate harmless error messages if the objects are not present initially

```
