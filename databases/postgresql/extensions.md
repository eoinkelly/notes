# Extensions

Postgres can load compiled C functions that extend its functionality.
That compbiled C function (called a "handler") can just be a wrapper around another language - this is how the perl/python etc. extensions are implemented.

It comes with 4 built in handlers

1. PL/pgSQL
1. PL/Tcl
1. PL/Perl
1. PL/Python

* A procedural language (PL) must be installled into a particular database before it can be used
* If you install it into `template1` then obviously it gets copied into all new databases

There are 2 ways to "install" a language

```sql
CREATE EXTENSION plperl;
```

```bash
$ createlang plperl template1
```

To create & use a function in Perl:

```sql
-- An example of installing perl language extension, creating a function in it
-- and using that function.

CREATE EXTENSION plperl;

CREATE FUNCTION perl_max (integer, integer) RETURNS integer AS $$
    if ($_[0] > $_[1]) { return $_[0]; }
    return $_[1];
$$ LANGUAGE plperl;


SELECT perl_max(4,17);
```
