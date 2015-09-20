## Databases and strings

##  MySQL

* very flexible: lets you mix and match character set and collation at
    server, db, table, column levels

## Postgres

[postgres docs](http://www.postgresql.org/docs/9.4/static/charset.html)

* Postgres sets locale info for a database at `initdb` time.
    * You can adjust most of these per query but the DB needs a fixed way to save them on disk
* It maintains an internal set of variables that map to the locale info provided by your shell
* Some locale categories have their values fixed when `initdb` is run
  because changing them would break ordering of indexes and text columns
    * LC_COLLATE
    * LC_CTYPE (a way to specialise LC_COLLATE, not often used)
* The other locale categories can be changed whenever desired by setting the server configuration parameters.

```
# you can specify locale at database create time
initdb --locale=sv_SE
```

* The locale affects
    * sort order of queries
    * whether LIKE, SIMILAR TO, regexps will match
    * upper, lower, initcap functions

WARNING: using custom locales has a performance impact!

> The drawback of using locales other than C or POSIX in PostgreSQL is its
> performance impact. It slows character handling and prevents ordinary indexes
> from being used by LIKE. For this reason use locales only if you actually
> need them


```sql
SHOW all; -- show all configuration

-- note the config values are lowercase
SHOW lc_collate;
SHOW lc_ctype;
```

### Collation support

* supports setting collation per column and even per operation
    * makes the fixed LC_COLLATE much less of a big deal

```sql
CREATE TABLE test1 (
    a text COLLATE "de_DE",
    b text COLLATE "es_ES",
    ...
);

-- use implicit collation of the column
SELECT a < 'foo' FROM test1;

-- override implicit collation with an explicit one
SELECT a < ('foo' COLLATE "fr_FR") FROM test1;

-- parser does not know which collation to apply so this is an error
SELECT a < b FROM test1;

-- can do something like
SELECT a < b COLLATE "de_DE" FROM test1;
-- to fix it
```

> The default character set is selected while initializing your PostgreSQL
> database cluster using initdb. It can be overridden when you create a
> database, so you can have multiple databases each with a different character
> set.

> An important restriction, however, is that each database's character set must
> be compatible with the database's LC_CTYPE (character classification) and
> LC_COLLATE (string sort order) locale settings. For C or POSIX locale, any
> character set is allowed, but for other locales there is only one character
> set that will work correctly.

You can see the encoding, ctype and collation of each table in a db using `\l` in psql
