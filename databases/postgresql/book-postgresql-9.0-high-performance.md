# Chapter 1

# Postgres Contrib

The contrib tools are porting tools, analysis utilities, and plug-in features
that are not part of the core PostgreSQL system.

- brew installs the postgres-contrib tools by default
- `apt-get install postgres-contrib` to get them on Ubuntu

Interesting tools

- Skytools
    - tools from Skype for replication and failover
    - https://wiki.postgresql.org/images/2/28/Moskva_DB_Tools.v3.pdf
- pgpool
    - connection pool manager
- PgBouncer
    - connection pool manager
- pb_buffercache
    - represents the shared buffers as a relation so you can query them
    - can see how much of a relation has been cached
    - this seems like a key tool when performance profiling a DB
    - http://www.postgresql.org/docs/9.4/static/pgbuffercache.html
- pbtune
    - python script to tweak your postgresql.conf based on detected system
      params
    - original script untouched since 2013
    - online version has seen more recent work https://github.com/le0pard/pgtune
- pgFouine
    - postgres log analyzer in PHP
    - untouched since 2010

Profiling a database usually starts with OS tools like:

- vmstat
- iostat

TODO: dig into those os level tools sometime (but not urgent) (Sun 29 Mar
00:28:54 2015)

General tips for performance tuning

- find the "current bottleneck" and fix that - the results of other fixes will
  not be visible
- Approach it at a system level not an individual app level

END CHAP 1
