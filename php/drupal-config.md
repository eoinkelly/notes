# Drupal config

* intended for storing values you want to migrate between systems
* use the State API if you want some state for just a single environment that should be thrown away outside that environment
* stored in yml and in the db
* Drupal Core has the idea of "Configuration storage"
    * active storage is in the database
    * you can export and import form it
    * the config is also cached
    * you also have "sync storage" which is on the filesystem
    * exporting from DB to files wipes all files first and then replaces them with data from DB
    * importing from files to DB is **merged** - it is not a wipe and replace


When the app requests some config it goes through the following layers:

```
Drupal app which consumes config
Overrides in settings.php
DB cache in cache_config (??? not sure how this fits in)
DB storage
```

When you import config from sync storage (yml files) to DB storage:

```
Sync storage(s) on filesystem -> Config filters layer -> DB storage
```

When you export config from DB storage (yml files) to sync storage:

```
DB storage -> Config filters layer -> Sync storage(s) on filesystem
```

## Database Tables

1. `cache_config`
    * not sure what this is for. if the config is already in the DB why also cache it?
    * the `config_*` tables don't have any indexes. this one has indexes on expiry and created timestamps
    * `created` timestamp stored as `DECIMAL(14,3)` - why???
2. `config`
    * stores the "active" config
    * this table is imported/exported via one of
      * drush commands
      * drupal console
      * "Configuration manager" module (a core Drupal module which provides admin GUI for working with config)
3. `config_import`
4. `config_export`
5. `config_snapshot`

The `config_*` tables seem to have different row counts but all have the same schema

### cache_config

* mostly key value pairs where the value is a PHP serialized class

```sql
CREATE TABLE `cache_config` (
  `cid` varchar(255) CHARACTER SET ascii COLLATE ascii_bin NOT NULL DEFAULT '' COMMENT 'Primary Key: Unique cache ID.',
  `data` longblob DEFAULT NULL COMMENT 'A collection of data to cache.',
  `expire` int(11) NOT NULL DEFAULT 0 COMMENT 'A Unix timestamp indicating when the cache entry should expire, or -1 for never.',
  `created` decimal(14,3) NOT NULL DEFAULT 0.000 COMMENT 'A timestamp with millisecond precision indicating when the cache entry was created.',
  `serialized` smallint(6) NOT NULL DEFAULT 0 COMMENT 'A flag to indicate whether content is serialized (1) or not (0).',
  `tags` longtext DEFAULT NULL COMMENT 'Space-separated list of cache tags for this entry.',
  `checksum` varchar(255) CHARACTER SET ascii NOT NULL COMMENT 'The tag invalidation checksum when this entry was saved.',
  PRIMARY KEY (`cid`),
  KEY `expire` (`expire`),
  KEY `created` (`created`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='Storage for the cache API.'
```


### config

```sql
CREATE TABLE `config` (
  `collection` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object collection.',
  `name` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object name.',
  `data` longblob DEFAULT NULL COMMENT 'A serialized configuration object data.',
  PRIMARY KEY (`collection`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='The base table for configuration data.'
```

### config_export

```sql
CREATE TABLE `config_export` (
  `collection` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object collection.',
  `name` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object name.',
  `data` longblob DEFAULT NULL COMMENT 'A serialized configuration object data.',
  PRIMARY KEY (`collection`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='The base table for configuration data.'
```

### config_import

```sql
CREATE TABLE `config_import` (
  `collection` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object collection.',
  `name` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object name.',
  `data` longblob DEFAULT NULL COMMENT 'A serialized configuration object data.',
  PRIMARY KEY (`collection`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='The base table for configuration data.'
```

### config_snapshot

```sql
CREATE TABLE `config_snapshot` (
  `collection` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object collection.',
  `name` varchar(255) CHARACTER SET ascii NOT NULL DEFAULT '' COMMENT 'Primary Key: Config object name.',
  `data` longblob DEFAULT NULL COMMENT 'A serialized configuration object data.',
  PRIMARY KEY (`collection`,`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='The base table for configuration data.'
```

## Config sync directory

* this dir is where drush will export your config to **and** import your config from
    * so you could ftp files into the sync dir and then import them from there
* drupal uses `sites/default/files/config_HASH` as the sync dir by default
* you can move it out of web root and are recommended to do so
* Pantheon puts it in `sites/default/config` because they have secured that dir (presumably in their nginx config)


* When a module/distribution/theme is **enabled** it's configuration is copied from its `config/install` directory into the drupal configuration.
    * Drupal only reads from the directory **once**, **when the module is first enabled!**
    * the yaml files in `config/install` are
        1. converted to assoc arrays
        1. have a `uuid` field added
        1. sometimes have a `new_revision` fields added ???
        1. PHP serialized into the `config` table with the YAML filename as the key

* Site UUID
    * stored in the `system.site` config item (i.e. `config/sync/system.site.yml`)
    * can also read with `drush cget system.site`
    * Drupal doe some kind of check of this UUID before importing new config
        * ??? not sure what exactly this is
        * ??? I presume all envs of the same site have the same UUID?

Module: config_readonly

* https://www.drupal.org/project/config_readonly
* prevent anybody from making config changes to config via the GUI
* locked in `settings.php`
* intended for you to lock production to force config changes to happen in a pre-prod env

Module: config_filter

* https://www.drupal.org/project/config_filter
* https://events.drupal.org/vienna2017/sessions/advanced-configuration-management-config-split-et-al
* inserts a fake storage between DB and sync storage (files) - essentially becomes a MITM for both export and import
* basis for other config modules

Module: config_split

* https://www.drupal.org/project/config_split
* https://events.drupal.org/vienna2017/sessions/advanced-configuration-management-config-split-et-al
* uses config_filter to split config into different sync stores

Module: config_ignore

* builds on config_filter
* https://events.drupal.org/vienna2017/sessions/advanced-configuration-management-config-split-et-al
* https://www.drupal.org/project/config_ignore

There is a version of the drupal installer which will install from a set of configuration files

## Config overrides in settings.php

* Configuration read from the DB can be overridden by `$config['settings']...` in `settings.php`
    * bit limited - you cannot add
        * new config fields
        * disable modules (pity)
    * you can override any of the config_* modules with this
    * all config flows through this from the DB before your app uses it


## exclude modules from settings import/export in settings.php

```php
// web/sites/example.settings.local.php

/**
 * Exclude modules from configuration synchronization.
 *
 * On config export sync, no config or dependent config of any excluded module
 * is exported. On config import sync, any config of any installed excluded
 * module is ignored. In the exported configuration, it will be as if the
 * excluded module had never been installed. When syncing configuration, if an
 * excluded module is already installed, it will not be uninstalled by the
 * configuration synchronization, and dependent configuration will remain
 * intact. This affects only configuration synchronization; single import and
 * export of configuration are not affected.
 *
 * Drupal does not validate or sanity check the list of excluded modules. For
 * instance, it is your own responsibility to never exclude required modules,
 * because it would mean that the exported configuration can not be imported
 * anymore.
 *
 * This is an advanced feature and using it means opting out of some of the
 * guarantees the configuration synchronization provides. It is not recommended
 * to use this feature with modules that affect Drupal in a major way such as
 * the language or field module.
 */
# $settings['config_exclude_modules'] = ['devel', 'stage_file_proxy'];
```