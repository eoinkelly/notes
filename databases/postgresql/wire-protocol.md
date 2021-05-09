# PostgreSQL wire protocol

Questions

Sources

* https://www.postgresql.org/docs/current/protocol-overview.html

Some features are available at both the protocol layer and the SQL layer e.g. extended query sub-protocol vs creating prepared statements and using cursors at the SQL layer. This can provide a work around if the driver doesn't support extended query.

Protocol has phases of operation

1. Startup phase
2. Normal operation phase
3. Termination

Assumptions

* Server can send messages to the client at any time so a pattern like a state machine is recommended for the client.
* Neither server nor client will send an incomplete message
    * If comms failure happens mid message then you have to abandon the connection because the protocol doesn't make it easy to pick up the message boundaries again

## Startup phase

* this part of the protocol is driven by the server (except for the intial connection)
Contains

1. Authentication
2.

## Normal operation

SQL commands are executed through one of the sub-protocols

1. Simple query
    * front-end sends a query string which is parsed and immediately executed by the backend
    * simple, less secure, less flexible, worse performance
2. Extended query
    * Query processing split into multiple steps:
        1. Parsing (textual-query --> [Parser] --> prepared-statement)
        2. Binding parameter values (prepared-statement --> [Binder] --> portal)
        3. Execution (portal --> [Executor] --> results)
    * More flexible, more secure, better performance, more complex
3. `COPY` operations protocol

### Extended query protocol

* State is maintained between steps via

1. Prepared statements
    * represents the result of 1) parsing and 2) semantic analysis of a query
        * _semantic analysis_ is basic syntax checking
    * are not ready to execute because they may be missing values for the parameters
    * can be multiple per session, referenced by their names
    * are never shared between sessions
    * are deleted at the end of the session
    * there is an unnamed" prepared statement
        * it's optimized differently to the named ones - the unnamed is optimized for single-use
2. Portals
    * represents a _ready to execute_ or _already partially executed_ statement
    * missing parameter values have been filled in
    * can be multiple per session, referenced by their names
    * are never shared between sessions
    * are deleted at the end of the session
    * there is an unnamed" prepared statement
        * it's optimized differently to the named ones - the unnamed is optimized for single-use

Terminology: Cursor vs Portal

* A portal is equivalent to a cursor for `SELECT` statements
* Cursors don't apply to other kinds of statements so the docs use _portal_

Steps in extended query
1. Parsing (textual-query --> [Parser] --> prepared-statement)
2. Binding parameter values (prepared-statement --> [Binder] --> portal)
3. Execution (portal --> [Executor] --> results)
    * can be told to fetch only a subset of results so multiple execute steps might be required

Formats

Currently only two formats are supported

1. Binary (format code = 1)
    * uses network byte order
    * can change between server versions (text format is more portable)
2. Text (format code = 0)
    * Embedded null characters are not allowed
        * No trailing null character in the strings. Frontend must add them if it needs them

The following things have a format code attached:

1. parameter value sent from frontend to backend
2. column of a query result

## Termination

* Usually initiated by the front-end
* Sometimes by the backend e.g.

The backend will roll back any incomplete transaction when the connection closes.

```
0x40 Startup message
0x51 Simple query

0x54 Row description
0x44 Data row
0x43 Command completion
0x5A Ready for query
```

Most messages follow the format:

    <message-type><4-byte-length><message-body>

but not all for some reason


* The length includes its own 4 bytes but not the message type byte
* strings are transmitted null terminated
    * it is common for the `<message-body>` to be a series of null terminated strings
    * can send multiple queries in one "simple query" message
        * if you type multipel queries on the same line in psql it will split them into multiple messages
* psql issues simple query messages

* Simple query is of the form `0x51<4-length-bytes><message-body>`
    * The message body is a series of null terminated strings representing the query
* Responses to queries are of the form:
    1. 1 x Row description message
    2. 0-N Data row messages
    3. 1 x Command completion
    4. 1 x Ready for query
* Postgres server will put multiple "postgres messages" within a single TCP packet when responding to your query


Avoiding SQLi


Ways:

1. When You client uses simple queries:
    * You must escape values properly
* If you client used extended query syntax:
    * the SQL code and parameters are sent in different messages so, provided your users cannot generate the SQL code string, then SQLi should be impossible.
    * Gotchas:
        * Don't build SQL from anything users can input


Conclusion: favour clients who use extended query syntax because it avoids the need for escaping data values but you still need to make sure that the _code_ you are sending the server doesn't get built from user inputted values
