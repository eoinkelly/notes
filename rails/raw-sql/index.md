# Managing raw SQL in Rails

- [Managing raw SQL in Rails](#managing-raw-sql-in-rails)
  - [Why use raw SQL?](#why-use-raw-sql)
  - [A suggested decision tree for navigating these architectural choices](#a-suggested-decision-tree-for-navigating-these-architectural-choices)
  - [This document assumes PostgreSQL](#this-document-assumes-postgresql)
  - [Background you need to understand your options](#background-you-need-to-understand-your-options)
    - [High-level API layers](#high-level-api-layers)
    - [Medium-level API layers (what we are interested in)](#medium-level-api-layers-what-we-are-interested-in)
    - [Low-level API layers](#low-level-api-layers)
  - [Writing raw SQL](#writing-raw-sql)
  - [Sending SQL to the database](#sending-sql-to-the-database)
    - [Option 1: Send as a regular query](#option-1-send-as-a-regular-query)
      - [Avoiding SQL injection with prepared statements](#avoiding-sql-injection-with-prepared-statements)
      - [Avoiding excessive memory usage with a CURSOR](#avoiding-excessive-memory-usage-with-a-cursor)
      - [Bulk transfer data to/from the DB with SQL COPY](#bulk-transfer-data-tofrom-the-db-with-sql-copy)
      - [ActiveRecord layer](#activerecord-layer)
        - [Public API](#public-api)
          - [1. ActiveRecord::Base.connection.execute --> PG::Result](#1-activerecordbaseconnectionexecute----pgresult)
          - [2. ActiveRecord::Base.connection.select_all --> ActiveRecord::Result](#2-activerecordbaseconnectionselect_all----activerecordresult)
          - [3. MyModel.find_by_sql --> Array\<MyModel\>](#3-mymodelfind_by_sql----arraymymodel)
        - [Private API](#private-api)
          - [4. ActiveRecord::Base.connection.exec_query --> ActiveRecord::Result](#4-activerecordbaseconnectionexec_query----activerecordresult)
          - [5. ActiveRecord::Base.connection.raw_connection --> PG::Connection](#5-activerecordbaseconnectionraw_connection----pgconnection)
      - [PG gem layer](#pg-gem-layer)
        - [PG::Connection#exec --> PG::Result](#pgconnectionexec----pgresult)
        - [PG::Connection#exec_params --> PG::Result](#pgconnectionexec_params----pgresult)
    - [Option 2: Send as database view](#option-2-send-as-database-view)
    - [Option 3: Send as materialised views](#option-3-send-as-materialised-views)
  - [Appendices](#appendices)
    - [Appendix: Do I need to call PG::Result#clear?](#appendix-do-i-need-to-call-pgresultclear)
    - [What is the equivalent of PG::Result#clear in MySQL](#what-is-the-equivalent-of-pgresultclear-in-mysql)
    - [Appendix: %q and %Q are handy when building large SQL strings](#appendix-q-and-q-are-handy-when-building-large-sql-strings)
    - [Appendix: `?` confusion (hint: it's not bound parameters in ActiveRecord)](#appendix--confusion-hint-its-not-bound-parameters-in-activerecord)
    - [Appendix: Debugging tips](#appendix-debugging-tips)
    - [Appendix: Santitizing SQL strings](#appendix-santitizing-sql-strings)
    - [Appendix: Alternatives to ActiveRecord](#appendix-alternatives-to-activerecord)
    - [Appendix: Suggested development workflow](#appendix-suggested-development-workflow)

## Why use raw SQL?

We want the following outcomes when we interact with a database:

1. We want the queries to be **fast enough**.
2. We don't want to allocate **too much** memory to perform the query and get the results into a usable form.
3. We want to be safe from SQL injection.

Most of the time, we can get these three outcomes from normal `ActiveRecord` methods. This document is about what to do when that doesn't work.

You will notice that the goals above have some squishy language about _fast enough_ and _not too much_ memory. You will need to decide what these terms mean for your team and your application.

Don't use raw SQL in Rails unless you **know** you currently have, or will have, a problem.

Don't use raw SQL in Rails just because you fear you _might_ have a problem. All architecture decisions have trade-offs. Here are some of the costs of using raw SQL in Rails:

1. The relationships between your tables, which normally lives in your application in the Rails relationships between them, is now duplicated in the SQL query. This means if you need to change the relationships in future, you will need to change both places.
2. Sometimes ActiveRecord will be faster than raw SQL. ActiveRecord (or any ORM) implements some kinds of caching which can speed up SQL queries which you application uses a lot.
3. Future maintainers have to know enough SQL to be able to maintain what you wrote.

## A suggested decision tree for navigating these architectural choices

    TODO: this could be diagram

1. Verify that you cannot achieve the outcomes you need with just normal ActiveRecord queries and you do actually need to write raw SQL.
1. Write the SQL query you want to use - see the dev tip below for
1. If the query needs parameters then decide whether to use a prepared statement at the SQL layer or at the wire protocol layer to protect it.
1. Decide whether that query should live in a _query object_, a _database view_ or a _materialised database view_.
1.  Decide whether you need to use a CURSOR to manage memory usage of the results

## This document assumes PostgreSQL

This document assumes PostgreSQL because that is what I know. A lot of this advice will also apply to MySQL but will differ in the lower level details.

## Background you need to understand your options

There are a few layers available to interact with the DB in a Rails app. This document is mostly about the middle layers

### High-level API layers

1. Standard everyday `ActiveRecord` methods
    * This doc ignores these but obviously you should try to find a solution with these first before resorting to raw SQL techniques.

### Medium-level API layers (what we are interested in)

1. ActiveRecord methods which let you pass raw SQL
1.  `pg` gem methods
    * sends raw SQL, gets back arrays of hashes
    * fields in the hashes have types set by the releveant pg typemap
    * seems to be a pretty close wrapper to the libpq API

### Low-level API layers

There are layers below what is accessible from rails:

1. libpq (C layer, not accessible from Rails)
    * C lib, ships with Postgres itself
    * Fully supports all Postgres features in V3 of the wire protocol
    * Almost all clients use this except:
      * ODBC (I _think_ - not entirely sure if the ODBC driver compiles it in?)
      * Javascript has a few packages which implement the wire protocol directly
        * https://github.com/panates/postgresql-client (Pure TS client)
        * https://github.com/brianc/node-postgres (optional libpq bindings)
1. Postgres wire protocol
   * It's never practical to go this low for real work.
   * I've found WireShark the best tool for actually seeing it in action


TODO: add background on the wire protocol versions, why `exec_params` is jargon you will see

## Writing raw SQL

Outside of scope of this doc

## Sending SQL to the database

Once you have written some SQL which performs the query you want, you have some choices about how to have the database invoke it.

### Option 1: Send as a regular query

There is an approach where you don't care too much about whether you are using the simple or extended wire protocol. Instead you solve everything in the SQL layer and just use `ActiveRecord::Base.connection.execute --> PG::Result` for all your raw SQL needs.

Pros/cons

* -- more complext SQL
* ++ more complex SQL might be easier to maintain than the exact tricks of the pg gem (maybe?)
* ++ you can avoid SQLi with SQL prepared statements
* ++ you can avoid too much memory usage with cursors or by manually batching your results somehow

TODO: use raw sql and prepare statements to implement a csv report generation with at least one parameter which could be open to sqli and returning a result set big enough to be a problem

Q: should you try to do everything at the SQL layer and ignore the other stuff?


what about database views and materialised views
    anything to do with this doc?

#### Avoiding SQL injection with prepared statements

#### Avoiding excessive memory usage with a CURSOR

#### Bulk transfer data to/from the DB with SQL COPY


#### ActiveRecord layer

Overview of the various methods of sending raw SQL using ActiveRecord methods.

##### Public API

###### 1. ActiveRecord::Base.connection.execute --> PG::Result

* returns `PG::Result`
* Always uses _Simple Query subprotocol_ at Postgres wire protocol level
* -- To avoid SQLi, you must either
    1. use `PREPARE...EXECUTE` in your SQL string to get separation
    1. OR just rely on escaping.
* Example: `ActiveRecord::Base.connection.execute(sql, name = nil)`
* Executes an SQL statement, returning a PG::Result object on success or raising a PG::Error exception otherwise. Note: the PG::Result object is manually memory managed; if you don't need it specifically, you may want consider the exec_query wrapper
* Does _Simple Query_ at Postgres protocol level
* example:
    ```ruby
    sql = "SELECT ..."
    pg_result = ActiveRecord::Base.connection.execute(sql)
    results = pg_result.values
    pg_result.clear

    results
    ```
* You can do a SQL level `PREPARE...EXECUTE` to get more safety with this method:
    ```ruby
    def do_safe_exec(sql_query, params)
        sql_query = "#{sql_query};" unless sql_query.ends_with?(";")

        stmt_name = "a#{Time.now.to_i}"
        unrolled_params = params.map { |param| "'#{param}'" }.join(",")

        prepared_sql = <<~EO_SQL
            PREPARE #{stmt_name} AS #{sql_query}
            EXECUTE #{stmt_name}(#{unrolled_params});
        EO_SQL

        pg_result = ActiveRecord::Base.connection.execute(prepared_sql)

        ActiveRecord::Base.connection.execute("DEALLOCATE #{stmt_name};")

        results = pg_result.to_a

        # Clear the memory associated with the PG::Result
        pg_result.clear

        results
    end
    ```
* A caveat to using execute is depending on your database connector, the result returned by this method might require manual garbage collection. Consider using exec_query instead.
    * TODO: docs mention this but I can't find examples of doing the cleanup

###### 2. ActiveRecord::Base.connection.select_all --> ActiveRecord::Result

* Documented in Rails guide
* Works a bit like `MyModel.find_by_sql` but does not instantiate ActiveRecord objects - it returns arrays of hashes wrapped in an `ActiveRecord::Result`
* returns: `ActiveRecord::Result`
* TODO: seems to support prepared statements
* layers:
    * pg gem layer: uses ???
    * libpq layer: calls ???
    * wire protocol layer: Does Parse/Bind/Describe/Execute/Sync
* ++ You can pass bound parameters
* -- It is extremely fiddly and poorly documented to pass bound parameters
* Example: `ActiveRecord::Base.connection.select_all`
* `ActiveRecord::Base.connection.select_all(arel, name = nil, binds = [], preparable: nil)`
* Note: lives on the connection adapter, not the model but can get at the instance of the connection adapter class via **any** model
        * e.g. `ActiveRecord::Base.connection == User.connection == Page.connection`
* Returns an instance of `ActiveRecord::Result` which can be converted to an array of hashes
* Just runs a SQL query and gives back data - it does not create ActiveRecord objects from that data. This makes it very useful for building reports if we don't need to do "Active Model" things with the returned data.
* Does Parse/Bind/Describe/Execute/Sync at the Postgres protocol level
* You can send bound parameters but it has the same ergonomic problems as `ActiveRecord::Base.connection.exec_query`

###### 3. MyModel.find_by_sql --> Array\<MyModel\>

* Documented in Rails guide
* Example: `SomeModel.find_by_sql`
* returns an actual `Array` of `SomeModel` objects (not an `ActiveRecord::Relation`)
* examples:
    ```ruby
    Page.find_by_sql("select * from pages where id = 3") # => Array<Page>

    # These also work (note that the ? and :foo are replaced by AR not
    # Postgres. All the `?` and `:foo` values are interpolated into the string
    # before it is sent to Postgres. The examples below send exactly the same SQL to Postgres
    Page.find_by_sql(["select * from pages where id = ?", 3])
    Page.find_by_sql(["select * from pages where id = :some_id", {some_id: 3}])
    ```
* Caveat: Not really suitable for doing INSERT/UPDATE/DELETE
* Does Parse/Bind/Describe/Execute/Sync at the Postgres protocol level
    * This is true even if you give it a single string of SQL with no interpolations
* You can send bound parameters but it has the same ergonomic problems as `ActiveRecord::Base.connection.exec_query`

##### Private API

###### 4. ActiveRecord::Base.connection.exec_query --> ActiveRecord::Result

* Returns instance of `ActiveRecord::Result` which can be converted to an array of hashes
    * ActiveRecords::Result object which has handy methods like .columns and .rows to access headers and values.
* Example: `ActiveRecord::Base.connection.exec_query(sql, name = "SQL", binds = [], prepare: false)`
* ++ Does Parse/Bind/Describe/Execute/Sync at the Postgres protocol level
* ++ You can pass bound parameters
* -- It is extremely fiddly and poorly documented to pass bound parameters
* Technically this is private API
* Executes sql statement in the context of this connection using binds as the bind substitutes. name is logged along with the executed sql statement.
* Uses Postgres Parse/Bind/Describe/Execute/Sync protocol messages under the hood
* Caveat: creating the bind parameters is really fiddly and uses a bunch of undocumented AR objects
    * at the pg gem level you can call exec_params and not pass in any types for the bind params but instead put explicit casts your sql but that doesn't seem to be allowed in this API?
* example:
    ```ruby
    # works
    full_sql = "select * from pages where template = 'topics'"
    ActiveRecord::Base.connection.exec_query(full_sql)
    # => ActiveRecord::Result

    # works
    sql = "select * from pages where template = $1"
    binds = [ActiveRecord::Relation::QueryAttribute.new("something", param_values.first, ActiveRecord::Type::String.new)]
    ActiveRecord::Base.connection.exec_query(sql, 'blah', binds)
    # => ActiveRecord::Result
    ```
* Conslusion: it is better to drop directly down to the PG gem layer instead of using this.

###### 5. ActiveRecord::Base.connection.raw_connection --> PG::Connection

* `ActiveRecord::Base.connection.raw_connection` is a `PG::Connection` i.e. it drops you down to the _pg gem layer_ (see below)

#### PG gem layer

To use API from this layer you need to get access to the `PG::Connection` from ActiveRecord (see above)

##### PG::Connection#exec --> PG::Result

* Use simple protocol
* You must use sanitization to avoid SQLi
* Don't use this if you can avoid it

##### PG::Connection#exec_params --> PG::Result

* Does Parse/Bind/Describe/Execute/Sync at the Postgres protocol level
* ++ You can pass bound parameters
* ++ Passing bound parameters is fairly straight forward
* Seems to work well for executing raw SQL in the safest manner possible


### Option 2: Send as database view

Recommendation: Use https://github.com/scenic-views/scenic to manage your views

Reasons to use a view

It lets you write raw SQL but then use normal activerecord methods to query it
    ++ maintainable for other devs
    ++ you can ignore the whole seciton about "how should I send my raw SQL"
++ view data is never out of date (compared to materialized view)
++ works great if query is fast enough for your need

### Option 3: Send as materialised views

Recommendation: Use https://github.com/scenic-views/scenic to manage your views

* ++ queries just as fast as any other table
* -- you need to be able to tolerate somewhat stale data
    * in a report export I guess you could refresh the view as first step but not sure how slow that would be?

How to setup triggers in rails to update it when any of the tables involved are updated?
* -- a cron job of some kind needs to run to update it

Examples: spreadsheet import or export

TODO: is SQL COPY usefule for this? I have never tried

## Appendices

### Appendix: Do I need to call PG::Result#clear?

> Explicit calling clear can lead to better memory performance, but is not
> generally necessary.
>
> https://deveiate.org/code/pg/PG/Result.html

### What is the equivalent of PG::Result#clear in MySQL

It's called `free`. See https://github.com/noahgibbs/mysql_bloat_test/blob/master/mb_test.rb

### Appendix: %q and %Q are handy when building large SQL strings

* Use %q or %Q to simplify quoting in long strings which contain single and double quotes e.g. SQL
* %q = create a string using "single quote rules" but you can choose your own
delimiter
* %Q = create a string using "double quote rules" but you choose your own
delimiter i.e. use %Q if you need variable interpolation
* examples
    ```ruby
    [22] pry(main)> x = 34
    34

    [23] pry(main)> %q[this is literally #{x}!]
    "this is literally \#{x}!"

    [24] pry(main)> %Q[this is literally #{x}!]
    "this is literally 34!"
    ```

### Appendix: `?` confusion (hint: it's not bound parameters in ActiveRecord)

ActiveRecord can interpolate values into a SQL string for you via

1. Placeholder maker `?` which causes AR to match up its usage to the position of an arg within an array
1. Named placeolder e.g. `:foo` which is used as the key in a hash to get the value to replace

The `?` syntax **looks like MySQL bound parameters** but using it does not mean that the query will use bound parameters when sent to the DB.

It does mean that you are telling AR to do escaping for you on the value which is probably safer than doing it yourself or (worst case) just interpolating it into the string yourself.

### Appendix: Debugging tips

Options

* Rails log file
    * usually this is all you need
    * ++ in development mode it's pretty good record of what queries are sent to DB
    * -- doesn't tell you whether Rails is using the simple or extended wire protocol for a particular method.
* Use Wireshark to spy on the traffic to/from the DB
    * ++ the wire protocol is the ground truth
    * ++ easy enough to understand the conversation between Rails and DB - messages are pretty sensible looking to human eyes
    * -- fiddly setup especially if you don't use Wireshark on the regular


### Appendix: Santitizing SQL strings

You should try use methods which use the extended query subprotocol (e.g. `PG::Connection#exec_params`) which is safer because it doesn't try to mash values into the SQL, but if you are stuck with doing that then Rails has a number of helpers which are useful.

There are different helpers depending on which clause of the SQL query you are inserting your string.

The helpers are documented in https://api.rubyonrails.org/classes/ActiveRecord/Sanitization/ClassMethods.html

Examples (from the docs):

```ruby
sanitize_sql_array(["name=? and group_id=?", "foo'bar", 4])
# => "name='foo''bar' and group_id=4"

sanitize_sql_array(["name=:name and group_id=:group_id", name: "foo'bar", group_id: 4])
# => "name='foo''bar' and group_id=4"

sanitize_sql_array(["name='%s' and group_id='%s'", "foo'bar", 4])
# => "name='foo''bar' and group_id='4'"
```

### Appendix: Alternatives to ActiveRecord

* https://github.com/discourse/mini_sql
    * wraps `pg` gem and provides some sugar
    * a collection of raw SQL helpers that Discourse created for themselves
    * does some statement caching so can be faster than `pg` in some cases
    * unknowns
      * what it does at the wire protocol layer

### Appendix: Suggested development workflow

You should use whatever development workflow you like best. I have found the following useful to get started on raw SQL in a Rails app.

I create a script which defines the SQL I need and runs it, then run that script in the Rails environment with [rails runner](https://guides.rubyonrails.org/command_line.html#bin-rails-runner)

Once the basic SQL statement is working, I can move it into a query object or Scenic view as required.

```ruby
# ./my_sql.rb

sql = ~<<EO_SQL
  SELECT * FROM ...
EO_SQL

# capture some timestamps for very basic profiling
start_ts = Time.zone.now

pg_result = ActiveRecord::Base.connection.execute(sql)

end_ts = Time.zone.now

# you don't **need** to do this in this script but it's a good habit to get
# into.
pg_result.clear

puts "Finished. Took #{end_ts - start_ts} secs"

```

Then in my terminals:

```bash
# terminal #1 ###########################
# load the Rails app and run ./my_sql.rb. This behaves a bit like a
# non-interactive version of `rails console`
$ bundle exec rails runner ./my_sql.rb


# terminal #2 ###########################
# Follow the output of the log file to see what SQL commands Rails is actually
# running.
$ tail -f log/development.log
```
