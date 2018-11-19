# Postgresql on macOS

Postgres on mac installs man pages for each of its keywords - it uses
underscore to join them together if required e.g. `man ALTER_TABLE`

# Upgrading major versions

* https://gist.github.com/eoinkelly/fd80465942c8ca4bd5c0

# Where stuff is on Mac (when Postgres installed via homebrew)

* Binaries are in `/usr/local/bin`
* "data dir" directory is `/usr/local/var/postgres`
    * contains config files
    * most databases in `/usr/local/var/postgres/base/`
        * each database has a subdir named after the database's OID in `/usr/local/var/postgres/base/`
    * can also be set with `PGDATA` environment variable or cmd-line option to the server
    * Log file: `/usr/local/var/postgres/server.log`
* More: http://www.postgresql.org/docs/9.4/static/storage-file-layout.html
