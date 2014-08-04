# Postgresql

## archive formats

Both `pg_dump` and `pg_restore` support a number of archive formats

* custom
    * understood by `pg_dump` and `pg_restore`
    * best choice if going from PG to PG
    * most flexible format
        * allows manual selection and reording of items during restore
    * compressed by default
        * how???
* directory
    * make
        * a directory for db,
        * one file for each table and blob being dumped
        * pg_restore readable table of contents file
    * compressed by default
    * supports parallel dumps - ???
* tar
    * use a tar file
    * if untarred is the same as a the `directory` format
    * does not support compression

## pg_dump

```
PGPASSWORD=mypassword pg_dump -Fc --no-acl --no-owner -h localhost -U myuser mydb > kete.pg_dump

# --format=? (-F)
    * select output format

# --format=c (-F)
    * select the custom-format archive
    * suitable for use with pg_restore,
    * compressed by default

--no-owner
  * don't output commands that set the ownership of objects to match the original DB
  * only meaningful for the `text` output format. pg_restore can control this if you use other formats

-h
  * host that DB is on

-U
  * database user

--no-acl
  * prevent dumping of access privileges

```

## pg_restore

```
 pg_restore --verbose --clean --no-acl --no-owner -h localhost -U myuser -d mydb latest.dump


pg_restore can figure out the format of the archive automatically

 --no-acl
  * do not restore access priveleges

--no-owner
  * do not restore the ownership of objects to match the original DB

--clean
  * Clean (drop) DB objects before recreating them
      * might generate harmless error messages if the objects are not present initially

```

