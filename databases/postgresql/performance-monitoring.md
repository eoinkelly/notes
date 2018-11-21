# Chapter 27 - Postgres monitoring

Tools to find out what PG is doing

1. OS tools: ps, top, iostat, and vmstat
2. EXPLAIN command to find out about a particular query
3. The statistics collector

# ps

`ps` can tell us a lot about what postgres is doing. There are 5 "helper" processes we
expect to always see:

1. master process
    * will show the command line that PG was started with
2. stats collector
    * if not show then it is configured to start manually
3. autovacuum launcher
    * if not show then it is configured to start manually
4. wal writer
    * write ahead log
5. checkpoint

and we also expect to see some database session processes

* one process per session
* format of the line is `postgres: {user} {database} {host} {activity}`
* If `update_process_title` is enabled (causes a lot of overhead in some systems) then activities can be
    * IDLE
    * IDLE IN TRANSACTION
    * command name .e.g. SELECT
* activity will have `waiting` appended if it is waiting for a lock from another session

```sh
➜  postgres git:(master) ps auxww | grep postgres
eoinkelly       83630   0.0  0.0  2617988    744   ??  Ss   Wed06am   0:00.54 postgres: eoinkelly eoin_play [local] idle
eoinkelly       83629   0.0  0.0  2618760    668   ??  Ss   Wed06am   0:00.06 postgres: eoinkelly eoin_play [local] idle
eoinkelly       83627   0.0  0.0  2614024    676   ??  Ss   Wed06am   0:00.04 postgres: eoinkelly postgres [local] idle
eoinkelly       14040   0.0  0.0  2687180      4   ??  Ss   Mon06am   0:03.70 postgres: eoinkelly eoin_play [local] idle
eoinkelly         600   0.0  0.0  2468140   1648   ??  Ss   Sun08am   1:51.47 postgres: stats collector process
eoinkelly         599   0.0  0.0  2612996   2608   ??  Ss   Sun08am   0:50.06 postgres: autovacuum launcher process
eoinkelly         598   0.0  0.0  2612996    112   ??  Ss   Sun08am   0:01.93 postgres: wal writer process
eoinkelly         597   0.0  0.0  2612996    152   ??  Ss   Sun08am   0:02.04 postgres: writer process
eoinkelly         596   0.0  0.0  2612996    284   ??  Ss   Sun08am   0:00.13 postgres: checkpointer process
eoinkelly         532   0.0  0.0  2612996    280   ??  S    Sun08am   0:38.32 /usr/local/opt/postgresql/bin/postgres -D /usr/local/var/postgres -r /usr/local/var/postgres/server.log

eoin@server1 ~ % ps auxww | grep postgres
postgres  1682  0.0  0.1 133948  9720 ?        S    Mar20   0:24 /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf
postgres  1763  0.0  0.5 133948 27936 ?        Ss   Mar20   0:06 postgres: checkpointer process
postgres  1764  0.0  0.4 133948 27428 ?        Ss   Mar20   1:05 postgres: writer process
postgres  1765  0.0  0.0 133948  2104 ?        Ss   Mar20   0:05 postgres: wal writer process
postgres  1766  0.0  0.0 134740  4604 ?        Ss   Mar20   0:11 postgres: autovacuum launcher process
postgres  1767  0.0  0.0 102568  1844 ?        Ss   Mar20   0:27 postgres: stats collector process
postgres 13152  0.9  0.8 156264 47240 ?        Ss   Mar23  56:51 postgres: deploy kete_production [local] idle
postgres 13646  0.9  0.7 150856 41232 ?        Rs   Mar23  55:40 postgres: deploy kete_production [local] SELECT
postgres 17817  0.9  0.9 159980 51000 ?        Ss   Mar24  36:59 postgres: deploy kete_production [local] idle

eoin@server2 ~ % ps auxww | grep postgres
postgres  2567  0.0  0.1 134984 10288 ?        S    Mar20   0:26 /usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf
postgres  2569  0.0  0.0 134984  4188 ?        Ss   Mar20   0:00 postgres: checkpointer process
postgres  2570  0.0  0.0 134984  1856 ?        Ss   Mar20   0:05 postgres: writer process
postgres  2571  0.0  0.0 134984  1844 ?        Ss   Mar20   0:05 postgres: wal writer process
postgres  2572  0.0  0.0 135776  2948 ?        Ss   Mar20   0:09 postgres: autovacuum launcher process
postgres  2573  0.0  0.0 103572  1812 ?        Ss   Mar20   0:14 postgres: stats collector process
postgres  2777  0.0  0.2 143548 12092 ?        Ss   Mar20   0:02 postgres: deploy nznavigator_production [local] idle
postgres  2889  0.0  0.2 143624 11808 ?        Ss   Mar20   0:01 postgres: deploy nznavigator_production [local] idle
postgres  3846  0.0  0.2 143488 11676 ?        Ss   Mar20   0:01 postgres: deploy nznavigator_production [local] idle


# docker container
root@a8a932f4071a:/# pstree -c
postgres─┬─postgres
         ├─postgres
         ├─postgres
         ├─postgres
         ├─postgres
         ├─postgres
         ├─postgres
         ├─postgres
         └─postgres
```

# View settings

* NB: postgres configuration files are usually not definitive
* SHOW ALL, SHOW {setting name}
    * good but they won't let you see if it is a session specific change
* `SELECT * FROM pg_settings;`
    * most comprehensive

```sql
-- show all ~240 settings
select * from pg_settings;

-- show current connections
select * from pg_stat_activity;
select * from pg_stat_activity where pid <> pg_backend_pid()  and usename = current_user;
```


# controlling session processes

* `max_connections` defaults to 100

> Generally, PostgreSQL on good hardware can support a few
> hundred connections. If you want to have thousands instead,
> you should consider using connection pooling software to
> reduce the connection overhead.

* PG will spread sessions across all CPUs but a single session can only use one
  CPU so a single complex query is not parallelized

# Seeing the exact command being executed by each server process in the cluster right now

It can be done. See 000-debugging-run-sheet.md

# Statistics collector

A sub-system that

1. collects
2. reports

information about server activity.

counts

* access to tables in disk block terms
* access to tables in row terms
* access to indexes in disk block terms
* access to indexes in row terms
* total no. of rows in the table
* info about the
    * vacuum section
    * analyze section
  of each table
* no of calls to user defined functions and the time spent in each one

There are a no. of views available to view stats on the cluster
    * full list: http://www.postgresql.org/docs/9.4/interactive/monitoring-stats.html
    * Remember that
        * these do not update instanteously
        * they will not update if you are within a transaction block so you don't
        have to worry about the numbers changing under you

TODO: Which of those tables is most useful (Sat 28 Mar 08:37:38 2015)

```sql
-- show the usage of all indexes in the current database
select * from pg_stat_all_indexes;
```

# pg_locks table

The `pg_locks` table will show you details of what locks are outstanding in the system. You can

* filter them by database or table or session
* figure out which database has the most ungranted locks - this might indicate that this DB is a source of contention among clients
* compare how much lock contention there is with how busy the DB is to figure out how traffic relates to locks for you

# Dynamic tracing

You can compile PG with `--enable-dtrace` to turn on probes and trace points
that DTrace can read. That is a pretty deep topic that I don't need right now.

END CHAPTER 27
