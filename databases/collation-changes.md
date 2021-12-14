

TIL that upgrading the major version of your OS can break PostgreSQL indexes and even cause data loss :scream:

**TL;DR dump and re-import your data when you dist-upgrade your OS.**

https://twitter.com/jer_s/status/1463709224697950210 is the tweet I saw. I'll try to give a bit of background to explain why this happens.

A _Collation_ is a **complex** set of rules for how the characters which make up a human language should be sorted. Over time, collations need to be changed - from the Unicode standard:

> there may be fixes needed as more information becomes available about languages; there may be new government or industry standards for the language that require changes; and finally, new characters added to the Unicode Standard will interleave with the previously-defined ones. This means that collations must be carefully versioned

Collation data files are provided by the Operating System. In the case of linux, they are part of `glibc` which is upgraded when you upgrade the major version of your operating system (aka "dist upgrade" in `apt` distros)

Postgres bakes the sort order of strings into a number of the data structures it creates e.g. always indexes, sometimes check constraints and partitions.
Postgres relies on the OS (`glibc`) for collation data so when collation data changes without warning, then the contents of the Postgres managed data structure (e.g. index) don't match the new collation rules Postgres is using. This can break your indexes, check constraints or partitions.
The catch is there isn't (currently) a 100% reliable way for Postgres to notice that collations have changed. They are working on improving this situation - see https://wiki.postgresql.org/wiki/Collations

The tweet links to https://github.com/ardentperf/glibc-unicode-sorting/ which enumerates how collation sort order has changed from Ubuntu 10.04 through 21.10. Notice that sometimes there are no changes but sometimes there are many. Just irregular enough to catch you out. So, for now at least, you should almost certainly dump and re-import your data when you do a major version upgrade of the OS that your database runs on.

There is a C library called `ICU` which implements collations - you can choose to use that in your Postgres database but it is not the default so most of us are probably not using it - see https://www.cybertec-postgresql.com/en/icu-collations-against-glibc-2-28-data-corruption/ for more info.

I haven't been able to find much info on whether MySQL has the same problem but https://ocelot.ca/blog/blog/2017/04/11/mysql-mariadb-international-components-for-unicode/ indicates that it does not.

> MySQL/MariaDB have their own code for collations while PostgreSQL depends on the operating system's libraries (libc etc.) to do all its collating with strcoll(), strxfrm(), and equivalents.
