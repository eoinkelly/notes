# Drupal 8 data storage
Drupal 8 has more kinds of data storage:

1. key/value store
    * State API
    * designed for "internal system state" - use configuration for admin managed state
    * `Drupal\Core\State\StateInterface` provides all the methods you need to interact with the State API
      * Implemented by the `State` service which you can inject or get globally via `\Drupal::state()`
            * TODO: why the backslash prefix?
    * stored in `key_value` table under the 'state' namespace
        * other subsystems use that table too with different namespaces

2. temp store
3. user data store
4. configuration
5. entities
    * `TypedData`
    * most regularly used by a dev
6. low level database API
    * much less used in D8 vs D7

## Configuration

* designed for storing everything that needs to be synced between environments
* a record of all the choices and config an admin might make in setting up a site and modules
* configuration is still stored primarily in the DB (called the _active storage_ but it can be exported/imported to YAML (called the _sync storage_)
* modules can provide their own chunks of yaml configuration which should be imported when they are installed
* configuration YAML can have schemas also defined in YAML

Workflow

1. you make edits in the UI
2. export the configuraiton to YAML (via UI or drush)
3. check that YAML into git
4. import the YAML in the next environment (via drush or UI)

You can choose where the sync directory is stored.

The default location for the "sync" directory is inside a randomly-named directory in the public files path.

```
drush config-export
drush config-import
```

There are

1. simple configuration
    * has one instance of it in the DB
2. configuraiton entities
    * has 0-N instances in the DB