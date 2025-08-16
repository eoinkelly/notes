```
# TODO: create a new db from cmdline
rake db:drop:all
rake db:create:all
pg_restore --verbose --clean --no-acl --no-owner -h localhost -U postgres -d db_name path/to/dump.pgdump
# edit config/database.yml to use the new db as development db
```
