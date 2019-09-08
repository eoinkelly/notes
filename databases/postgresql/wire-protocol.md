

These notes are bit random, just some interesting stuff I discovered while playing around.

https://www.postgresql.org/docs/current/protocol-overview.html

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
    1. 0-N Data row messages
    1. 1 x Command completion
    1. 1 x Ready for query
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
