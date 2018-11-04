# Heroku cheatsheet

```
# HEROKU_APP=blah heroku help <command>

heroku pg:backups:capture # make a backup of the DB at HEROKU_DATABASE_URL
heroku pg:backups:restore HEROKU_POSTGRESQL_AMBER_URL # restore it to the given URL


heroku pg:promote HEROKU_POSTGRESQL_AMBER_URL # make HEROKU_POSTGRESQL_AMBER_URL DB the primary one


heroku ps:restart # restart all dynos

heroku config # show the config on server
```

## migrating a database from one heroku to another


1. Option: Copy all data to local pg_dump file via pg_dump
1. Option: Copy all data to local pg_dump file via heroku command line tool
1. Option: Copy all data directly from one heroku db to another
