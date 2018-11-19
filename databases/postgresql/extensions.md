# Extensions

Sources

* https://www.postgresql.org/docs/9.5/external-pl.html

Background

* Postgres can load compiled C functions that extend its functionality.
* That compbiled C function (called a "handler") can just be a wrapper around
  another language runtime - this is how the perl/python etc. extensions are
  implemented.

It comes with 4 built in handlers

1. PL/pgSQL
2. PL/Tcl
3. PL/Perl
4. PL/Python

Others are available

1. Ruby
    * https://github.com/knu/postgresql-plruby
2. Javascript
    * https://github.com/plv8/plv8

* A procedural language (PL) must be installled into a particular database before it can be used
* If you install it into `template1` then it gets copied into all new databases

Use `CREATE EXTENSION` to "install" a language

Example

```sql
-- An example of installing perl language extension, creating a function in it
-- and using that function.

-- installs the Perl extension
CREATE EXTENSION plperl;

CREATE FUNCTION perl_max (integer, integer) RETURNS integer AS $$
    if ($_[0] > $_[1]) { return $_[0]; }
    return $_[1];
$$ LANGUAGE plperl;


SELECT perl_max(4,17);
```

Docker postgres only includes PL/pgSQL - you have to install Perl/Python/Tcl manually e.g.

```Dockerfile
FROM postgres:9.5

RUN apt-get update \
    && apt-get install -y \
        postgresql-plperl-$PG_MAJOR=$PG_VERSION \
        postgresql-plpython-$PG_MAJOR=$PG_VERSION \
        postgresql-pltcl-$PG_MAJOR=$PG_VERSION \
    && rm -rf /var/lib/apt/lists/*
```

## RDS

You can see what extensions RDS supports via psql:

```sql
SHOW rds.extensions;
```

There are tables in the middle of https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html which detail what extensions are supported on each version of Postgres.

PG11 on RDS seems to support pgSQL, Perl, Tcl, Javascript