# Postgresql

# Upgrading major versions

* https://gist.github.com/eoinkelly/fd80465942c8ca4bd5c0

# Where stuff is on Mac

* "data dir" directory is `/usr/local/var/postgres`
    * contains config files and most databases in `base/`
        * each database has a subdir named after the database's OID in `pg_database`
    * can also be set with PGDATA environment variable or cmd-line option
    * Log file: /usr/local/var/postgres/server.log
* Binaries are in `/usr/local/bin`
* More: http://www.postgresql.org/docs/9.4/static/storage-file-layout.html

# authorisation

