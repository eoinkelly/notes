# Postgres perforamnce tips

## Sources

* http://confreaks.com/videos/3338-railsconf-biggish-data-with-rails-and-postgresql
* https://speakerdeck.com/snhorne/biggish-data-with-rails-and-postgresql
* https://bitly.com/bundles/o_3olsgfc0i1/1

## Perf tips

* Use a real server not a VPS
* The name of the game in postgres performance is to **limit the no. of rows you
  touch**. The query plan will tell you this.
* Increase read ahead cache on linux
  * default is 256kb
  * `blockdev --setra 2048 /dev/sda`
* use a modern linux filesystem
  * don't use ext3
  * use ext4 or xfs(??)
  * be careful if you have journalling in ext4 as pg does its own journaling
* use pg-tune script https://github.com/gregs1104/pgtune
* use vacuum
  * https://devcenter.heroku.com/articles/heroku-postgres-database-tuning
  * vacuum is resource intensive
  * "usually the answer to vacuum problems is to do it more often not less"

If you have a *lot* of data coming in you need to watch for:

* too many db connections
  * each db connection is its own process (with the usual RAM overhead)
  * => you need a finite no. of connections
  * http://wiki.postgresql.org/wiki/Replication,_Clustering,_and_Connection_Pooling
  * pgbouncer proxy that functions as connection pool
* too many connections trying to write to the same row at the same time
  * https://wiki.postgresql.org/wiki/Lock_Monitoring
  * especially rails counter cache: parent model has a counter row where it
    keeps track of how many children models it has - if many children get
    created at once this will cause problems

If you have intensive queries you can used pg replication to create a read-only streaming replicant and do the intensive work on that - this leaves your master db free to process writes

* http://www.postgresql.org/docs/current/static/warm-standby.html

Postgres partitioning is great for making archival and deletion of large data
sets fast.

* https://github.com/keithf4/pg_partman
* can setup partitioning so data for different days go into different physical table
* postgres manages making the partitions appear as one table to your ruby code

Backups take forever if you have a huge dataset. Use wal-e to do continious
arichiving https://github.com/wal-e/wal-e

From https://www.amberbit.com/blog/2014/2/4/postgresql-awesomeness-for-rails-developers/ these are postgres config settings worth tweaking

```sh
listen_addresses = '*' # To which interface we should bind. '*'
                    # makes your PostgreSQL visible to the Internet

max_connections = 200  # How many connections we should allow from
                       # our app, workers, delayed_jobs etc. combined
shared_buffers = 16GB  # How much memory our PostgreSQL can use for
                       # buffers. Default value is insanely small.
                       # If PostgreSQL is the only thing we run on
                       # the machine, set it to 1/4 of available RAM
work_mem = 10MB    # Increase the small value so the
                   # sorts perform better.
maintenance_work_mem = 128MB

synchronous_commit = off # Speed up writes in exchange for possible
             # loss of data (be careful here!)

wal_buffers = 16MB # Basically how much data we can loose. But
           # increasing makes things faster. Choose wisely.
           # Also applies to settings below.
wal_writer_delay = 400ms
checkpoint_segments = 32
checkpoint_timeout = 900s
checkpoint_completion_target = 0.9

random_page_cost = 2.0 # Make planner use indices a bit more often
             # vs. sequential table scans.

effective_cache_size = 32GB  # How much memory in total our
               # PostgreSQL can use. Twice of
               # shared_buffers seems good.
```

Recommended book: https://www.packtpub.com/big-data-and-business-intelligence/postgresql-replication
