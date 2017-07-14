## SQLcl

* is a superset of `SQL*Plus`

> Does it require an Oracle Client?
>
> No. SQLcl is a java applications and by default uses Oracle’s JDBC driver to
> connect to Oracle Database. However, you can force a ‘thick’ connection via
> your Oracle Client by using the –oci flag on your connect string

> SQLcl IS SQL Developer, just with a CLI vs a GUI. You can for example use the
> DDL and INFO commands in SQL Developer's worksheet, running as a script (F5)

> The new take on SQLPlus, SQLcl, is based on the script engine in Oracle SQL
> Developer and is attached to a Java-based command-line interface. In addition
> to delivering a more modern way of working on the command line, SQLcl also
> introduces new commands and features missing from SQLPlus itself.

> SQLcl supports connections via EZConnect, TNS, LDAP, TWO_TASK, and more—and
> all without an Oracle client installed or configured

TODO: what are those acronyms?

http://www.oracle.com/technetwork/issue-archive/2015/15-sep/o55sql-dev-2692807.html

```
./sqlcl/bin/sql --help
./sqlcl/bin/sql system/oracle@localhost
```

```
exit
```

    The AS SYSDBA, AS SYSOPER and AS SYSASM  options are database administration privileges.

