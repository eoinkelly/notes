
```
SQL>help set sqlformat # list output formats
SQL>show sqlformat
SQL>set sqlformat ansiconsole -- decent terminal output
-- you can output as JSON, CSV etc.

-- find out versions of various oracle bits
SQL> SELECT * FROM v$version; -- show DB version
SQL> select * from PRODUCT_COMPONENT_VERSION;

select username from dba_users;

select tablespace_name, table_name from all_tables;
select tablespace_name, table_name from user_tables;
select tablespace_name, table_name from dba_tables;

desc NAME_OF_TABLE; -- describe a table


sql /nolog # start SQLcl but don't connect
SQL>connect USERNAME
SQL>connect USERNAME AS
# CONN[ECT] [logon] [AS {SYSOPER | SYSDBA | SYSBACKUP | SYSDG | SYSKM}]
# {username | /}[@connect_identifier] [edition={edition_name | DATABASE_DEFAULT}]
```


* `v$version` is a special table name

* a schema is created in Oracle when a user is created.
    * there is a `CREATE SCHEMA` statement but it just creates multiple objects at once


Multi-tenant

* CBD = the "multitenant container DB"
    * multitenant container database (CDB) that includes one or many customer-created pluggable databases (PDBs).
* PDB = pluggable DB = the customer's DB
    * you can set it up so when the customer connects they only see their PDB
    * A PDB is a portable collection of schemas, schema objects, and nonschema objects that appears to an Oracle Net client as a non-CDB.
* All Oracle databases before Oracle Database 12c were non-CDBs.
* You can unplug a PDB from a CDB and plug it into a different CDB

To use `SQL*Plus`

> on most platforms, ORACLE_SID and ORACLE_HOME must be set. In addition, it is advisable to set the PATH environment variable to include the ORACLE_HOME/bin directory.

> UNIX and Linux installations come with two scripts, oraenv and coraenv, that you can use to easily set environment variables
