# Drush

Q; is drush run locally or on server?

## Drush and Drupal

Drupal 7: Use up to Drush 7 Drupal 8.0 -> 8.3: Use Drush 8 Drupal 8.4 -> 9: Use
Drush 10

## Features

- aliases
    - can run define remote servers as aliases and run commands against them
        - I assume this ssh'es in and runs them???
    - you can define site aliases to work with multiple sites in one drupal
      installation
- drush can login with ssh or rsync or docker exec
- drush requires a `sites/default/settings.php` exist in the dir you give it
  (cwd by default) to know there is a drupal installation
- command line shell for drupal
- it can do many things (its kind of a mix of capistrano, rake rails \*, etc.)
    - run db updates
    - import config
    - clear cache
    - modules can extend drush with their own commands
    - run generators (Quickly build a Drupal module, controller, plugin, entity,
      migration)
        - builds on https://github.com/Chi-teck/drupal-code-generator project

## Install drush

```bash
$ composer init # if project doesn't have a composer.json

$ composer require drush/drush
# takes quite a while

$ vendor/bin/drush --version
 Drush Version   :  7.4.0
```

Examples of drush commands:

```bash
-y # answer yes to all questions

drush core-status

#  updatedb (updb)       Apply any database updates required (as with running update.php).
#  features-revert (fr)  Revert a feature module on your site
#  cache-clear (cc)      Clear a specific cache, or all drupal caches.
#  features-diff (fd)    Show the difference between the default and overridden state of a feature.
#  variable-set (vset)   Set a variable.
```
