## Upgrading Postgres

- PG changes data formats between "minor" revision numbers (minor in semver,
  "major" in PG culture)

There are 3 main options

1. dump and restore
    1. dump with current version
        - `pg_dump`
            - ++ exports to binary or SQL formats
            - -- only works on a single database
        - `pg_dumpall`
            - -- can only export to SQL
            - ++ dumps all databases as a single SQL file
    2. upgrade to new version (with new tools)
    3. restore with new version
        - pg_restore can do parallel restores in 9.x +
2. in-place upgrade with `pg_upgrade`
    - -- you still need old and new versions installed at same time
    - used to be called `pg_migrator`
    - -- there is more risk with in-place upgrade than dump and restore
    - `pg_upgrade --help` for details of how to use it
        - you still have to manually create the new cluster using the new
          version of `initdb`
3. use statement replication to bring up a new version
    - slony does _statement replication_ and can go across PG versions
    - ++ you just wait for the replica to catch up and the flip it over w. very
      little downtime
