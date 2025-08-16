# PostgreSQL wire protocol

Questions

Sources

- https://www.postgresql.org/docs/current/protocol-overview.html

Some features are available at both the protocol layer and the SQL layer e.g.
extended query sub-protocol vs creating prepared statements and using cursors at
the SQL layer. This can provide a work around if the driver doesn't support
extended query.

Protocol has phases of operation

1. Startup phase
2. Normal operation phase
3. Termination

Assumptions

- Server can send messages to the client at any time so a pattern like a state
  machine is recommended for the client.
- Neither server nor client will send an incomplete message
    - If comms failure happens mid message then you have to abandon the
      connection because the protocol doesn't make it easy to pick up the
      message boundaries again

## Startup phase

- this part of the protocol is driven by the server (except for the initial
  connection)
- Contains
    1. Authentication
    2. ???

## Normal operation

SQL commands are executed through one of the sub-protocols

1. Simple query
    - front-end sends a query string which is parsed and immediately executed by
      the backend
    - simple, less secure, less flexible, worse performance
2. Extended query
    - Query processing split into multiple steps:
        1. Parsing (textual-query --> [Parser] --> prepared-statement)
        2. Binding parameter values (prepared-statement --> [Binder] --> portal)
        3. Execution (portal --> [Executor] --> results)
    - More flexible, more secure, better performance, more complex
    - GOTCHA: A client can use the extended query sub-protocol in a way that
      still allows SQLi - they could just inline their data values in the SQL
      passed to the Parsing step - if it is syntactically valid then Postgres
      will run it
3. `COPY` operations protocol

### Extended query protocol

State is maintained between steps via

1. Prepared statements
    - represents the result of 1) parsing and 2) semantic analysis of a query
        - _semantic analysis_ is basic syntax checking
    - are not ready to execute because they may be missing values for the
      parameters
    - can be multiple per session, referenced by their names
    - are never shared between sessions
    - are deleted at the end of the session
    - there is an "unnamed" prepared statement
        - it's optimized differently to the named ones - the unnamed is
          optimized for single-use
2. Portals
    - represents a _ready to execute_ or _already partially executed_ statement
    - missing parameter values have been filled in
    - can be multiple per session, referenced by their names
    - are never shared between sessions
    - are deleted at the end of the session
    - there is an unnamed" prepared statement
        - it's optimized differently to the named ones - the unnamed is
          optimized for single-use

Terminology: Cursor vs Portal

- A portal is equivalent to a cursor for `SELECT` statements
- Cursors don't apply to other kinds of statements so the docs use the term
  _portal_

Steps in an extended query

1. Parsing (textual-query --> [Parser] --> prepared-statement)
2. Binding parameter values (prepared-statement --> [Binder] --> portal)
3. Execution (portal --> [Executor] --> results)
    - Server can be told to fetch only a subset of results so multiple execute
      steps might be required

Formats

Currently only two formats are supported:

1. Binary (format code = 1)
    - uses network byte order
    - can change between server versions (text format is more portable)
2. Text (format code = 0)
    - Embedded null characters are not allowed
        - No trailing null character in the strings. Frontend must add them if
          it needs them

The following things have a format code attached:

1. parameter value sent from frontend to backend
2. column of a query result

The format decides whether the server should interpret the raw value bytes from
the client as text|binary.

After that, the bytes have to be turned into a Postgres native type. Each DB
column has a type with defined input/output functions which can coerce an input
value to be the correct DB type. This is similar to what would happen if you
typed SQL into psql

## Termination

- Usually initiated by the front-end
- Sometimes by the backend e.g.

The backend will roll back any incomplete transaction when the connection
closes.

```
0x40 Startup message
0x51 Simple query

0x54 Row description
0x44 Data row
0x43 Command completion
0x5A Ready for query
```

Most messages (but not all) follow the format:

    <message-type-byte><4-byte-length><message-body>

- The length includes its own 4 bytes but not the message type byte
- strings are transmitted null terminated
    - it is common for the `<message-body>` to be a series of null terminated
      strings
    - can send multiple queries in one "simple query" message
        - if you type multiple queries on the same line in psql it will split
          them into multiple messages
- **psql issues simple query sub-protocol messages** (it does not use the
  extended protocol)

    TODO: can psql use extended?

Simple query message format:

- Simple query is of the form `0x51<4-length-bytes><message-body>`
    - The message body is a series of null terminated strings representing the
      query
- Responses to queries are of the form:
    1. 1 x Row description message
    2. 0-N Data row messages
    3. 1 x Command completion
    4. 1 x Ready for query
- Postgres server will put multiple "postgres messages" within a single TCP
  packet when responding to your query

Avoiding SQLi

Ways:

1. When your client uses simple queries:
    - You must escape values properly before sending them

- If you client used extended query syntax:
    - the SQL code and parameters are sent in different messages so, provided
      your users cannot generate the SQL code string, then SQLi should be
      impossible.
    - Gotchas:
        - Don't build SQL from anything users can input

Conclusion: favour clients who use extended query syntax because it avoids the
need for escaping data values but you still need to make sure that the _code_
you are sending the server doesn't get built from user inputted values

## Copy sub-protocol

> Copy-in and copy-out operations each switch the connection into a distinct
> sub-protocol, which lasts until the operation is completed.

https://www.postgresql.org/docs/current/protocol-flow.html#PROTOCOL-COPY

Copy-in mode (transferring data **to** the server):

1. Backend executes a `COPY FROM STDIN` SQL statement
1. Backend enters copy-in processing mode
1. Backend (server) sends `CopyInResponse` message to frontend
1. Frontend sends 0+ `CopyData` messages (usually but not required to be one per
   row)
1. Frontend sends either `CopyDone` (for success) or `CopyFail` (failure)
1. Backend reverts back to the command processing mode it was in before moving
   to copy-in mode

Copy-out mode (transferring data **from** the server)

1. Backend executes a `COPY TO STDOUT` SQL statement
1. Backend enters copy-out processing mode
1. Backend (server) sends `CopyOutResponse` message to frontend
1. Backend sends 0+ `CopyData` messages (always one per row)
1. Backend sends either `CopyDone` (for success) or `ErrorResponse` (failure)
1. Backend reverts back to the command processing mode and issues
   `CommandComplete`

Copy-both mode

> Copy-both mode is initiated when a backend in "walsender" mode executes a
> START_REPLICATION statement
