## Vacuuming

* PostgresSQL is a transactional database, old rows don't get actually
  removed/replaced when you update/delete them (since they might be still
  needed in older/long running transactions).
* To actually free them you need to issue a vacuum.  A normal vacuum will only
  mark deprecated rows for reuse, to actually reclaim diskspace (e.g. when
  having deleted large amounts of data) you need to issue a full vacuum.
* it might be faster to backup the data you want to keep and truncate the table
  if you plan to remove large portions of a table.

