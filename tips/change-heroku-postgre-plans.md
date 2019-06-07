

```bash
# capture a backup for safety
HEROKU_APP=myapp heroku pg:backups:capture

# In Heroku web console, provision the desired database as a new database on the app.
# Then note the name of the new db e.g. HEROKU_POSTGRESQL_PINK.
# Then wait for it to provision properly (this takes bit longer for the more expensive dbs)

# go into maintenance mode to stop new writes
HEROKU_APP=myapp heroku maintenance:on

# copy the database to the new one
HEROKU_APP=myapp heroku pg:copy DATABASE_URL HEROKU_POSTGRESQL_PINK

# promote the new DB to be the main DB for the app
HEROKU_APP=myapp heroku pg:promote HEROKU_POSTGRESQL_PINK

# Note the name of the old DB will now have a color e.g. LAVENDER
# come out of maintenance mode
HEROKU_APP=myapp heroku maintenance:off

# deprovision the old db
HEROKU_APP=myapp heroku addons:destroy HEROKU_POSTGRESQL_LAVENDER
```
