# CSV Export best practices

there are layers available to interact with the DB

1. activerecord or other ORM
2. pg gem
    - sends raw SQL, gets back arrays of hashes
    - fields in the hashes have types set by the releveant pg typemap
    - seems to be a pretty close wrapper to the libpq API
3. libpq (C layer)
    - C lib, shpped with Postgres itself
    - Fully supports all Postgres features in V3 of the wire protocol
    - Almost all clients use this except:
        - ODBC (I _think_ - not entirely sure if the ODBC driver compiles it
          in?)
        - Javascript has a few packages which don't use it _
          https://github.com/panates/postgresql-client (Pure TS client) _
          https://github.com/brianc/node-postgres (optional libpq bindings)
4. wire protocol
    - It's never practical to go this low for real work.

Some features provided by libpq (e.g. prepare statements or execparams) are also
available in the SQL layer so even if you can't control which libpq function is
called you can still use the feature.

questions

? are sql cursors useful here? TODO check this out ? is SQL COPY useful here?

strategies

synchronous use activerecord, making sure to use includes to make efficient sql
++ ok in many cases ..escalate to.. write custom SQL to get the results --
duplicates AR logic in a SQL query ..escalate to.. create a view in the DB
..escalate to.. ++ fast -- duplicates AR logic in a SQL query create a
materialised view in the DB ++ fast -- duplicates AR logic in a SQL query --
materialised view needs to be kept up to date

asynchronous do export in background job then send email to notify AND/OR put
export on disk and allow user to download from a list of "last N exports" AND/OR
have front-end poll and endpoint to know when the export is done still need some
state in the db to capture when it's done
