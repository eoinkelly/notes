# Official Postgres docs

## Preface

* Started life at Berkeley as POSTGRES (up to version 4.2)
    * Had its own query lang POSTQUEL (libpq is still named after this)
* Then POSTGRES95
    * A refactor into ansi C
    * Started to support more SQL
* Then PostgreSQL rename
    * To indicate it supported SQL now

Conventions

* `[]` indicate an optional
* `{}` indicate a required option
* `|` separates alternatives
* `...` indiates previous option can be repeated

## Tutorial

* Postgres is client server model
    * server process is called `postgres`
* Postgres will **fork a new process for each connection**
    * Once the master process has forked a worker all communication between the client and worker happens directly - the master is not involved anymore

UP TO 1.3. CREATING A DATABASE

