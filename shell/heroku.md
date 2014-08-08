# Heroku cheatsheet

```
heroku pgbackups:capture # make a backup of the DB at HEROKU_DATABASE_URL
heroku pgbackups:restore HEROKU_POSTGRESQL_AMBER_URL # restore it to the given URL


heroku pg:promote HEROKU_POSTGRESQL_AMBER_URL # make HEROKU_POSTGRESQL_AMBER_URL DB the primary one


heroku ps:restart # restart all dynos

heroku config # show the config on server
```

