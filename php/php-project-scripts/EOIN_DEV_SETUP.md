# Installing on Eoin's machine

```bash
composer install

# apache version (not used now)
# vim ~/my-custom-httpd.conf # change Documentroot
# sudo apachectl restart

mysql -u root -h 127.0.0.1 -p -D drupal_dpc < ./dpc_community_dump.sql

# create nginx.conf
nginx -t ./nginx.conf # test the config and exit

# terminal 1:
nginx -p . -c ./nginx.conf # run the server, dumping errors to stderr

# terminal 2
php-fpm -F

# terminal 3 (optional)
tail -f log/access_log

cd profiles/collabco/themes/custom/collabco_theme/gulp
npm i
gulp
```

## Create settings.php

The best template for settings.php is the one in ansible, not in the standard drupal places

Add to sites/default/settings.php:

```php
// sites/default/settings.php

$databases = array (
  'default' =>
  array (
    'default' =>
    array (
      'database' => 'drupal_dpc',
      'username' => 'root',
      'password' => 'password',
      'host' => '127.0.0.1',
      'port' => '',
      'driver' => 'mysql',
      'prefix' => '',
    ),
  ),
);

```

## Setup .envrc

```bash
export AWS_ACCESS_KEY_ID=""
export AWS_SECRET_ACCESS_KEY=""
export PATH=./vendor/bin:$PATH
```

## Set up your admin user

```bash
drush user-password user+1@localhost.localdomain --password="whatever"
```
