# Rails and Postgres

* each thread _or_ process requires its own connection to the database
* we usually start 3 unicorn processes so we make 3 connections to the DB and
  hence 3 session processes
* a connection pool size of 5 does not mean that there will always be 5 opens
  connections - just that AR will create connections as it needs them up to the
  limit.
* connections are "checked out" by a request and "checked in" when it is done
* the "pool size" is for each thread or process i.e. 3 unicorn processes *can*
  open up to 15 connections to Postgres
* Puma spawns 16 threads by default so would need a pool size of 16 to work
* If you try to use more connections than are available, Active Record will
    block and wait for a connection from the pool. When it cannot get a
    connection, a timeout error will be thrown.
    ```
    ActiveRecord::ConnectionTimeoutError - could not obtain a database connection
    within 5 seconds. The max pool size is currently 5; consider increasing it
    ```
* A single threaded server can only process one request at a time so it only
  needs one connection
* 5 connections allows for 3 unicorns, and a `rails console` or `rails db` too
* The connection pool is established in a Rack handler so it can be maintained
  across requests so you don't have the overhead of setup and teardown of the
  connection on each request - this makes the pool useful even for single
  threaded servers
* Rails 4 can "reap" hung connections but it is turned off by default

## How to customise connection settings

1. add a `pool` attribute to appropriate section of `database.yml` (rails 4.1+)
2. use an initializer (older rails)

See https://devcenter.heroku.com/articles/concurrency-and-database-connections

# Custom commands in migrations

NB: If you do custom things in your migrations you need to change the schema format to SQL not ruby!

The main way is to call the `execute` method with a string of SQL to execute

```ruby
execute "ALTER TABLE blah ...."
```

