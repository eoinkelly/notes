# How-to set up a new PHP project on my laptop

# THIS IS DEPRECATED NOW - USE DOCKER INSTEAD

## Overview

* PHP
    * Managed by phpenv
* phpenv
    * Installed manually - see https://github.com/phpenv/phpenv
* MySQL server
    * Installed from docker
* MySQL client
    * Installed from homebrew: `brew install mysql` (this also installs the server but we don't start it so we can ignore it)
* nginx
    * Installed from homebrew
    * every project gets it's own nginx master process

I like to do dev in a split window terminal with all the relevant processes running under my direct control and (where possible) logging output and errors to the terminal window.

## Diagnose

```sh
# show installed versions of PHP
$ phpenv versions

# Dump the config used to build the current version of PHP
$ php-config

# see if the databases are up
$ docker ps
```

## Step: Choose app dir and set up direnv

Create a `.envrc` file (not required but convenient)

```sh
# ./.envrc

# allows you to avoid having to add "-h 127.0.0.1" flag to the mysql client command line
export MYSQL_HOST="127.0.0.1"
```

## Step: Set up MySQL

* See https://hub.docker.com/_/mysql for documentation of the env vars we pass in to the docker containers

```bash
# run latest MySQL on the standard port
$ docker run --name mysql --detach -p "3306:3306" --restart=always -e MYSQL_ROOT_PASSWORD=password -d mysql:latest

# OR for Legacy Drupal:
# NB: Drupal 7 does not support MySQL 8 so we need to use 5.7
# NB: Server will be running on port 3307
$ docker run --name mysql5_7 --detach -p "3307:3307" --restart=always -e MYSQL_ROOT_PASSWORD=password  mysql:5.7
```

```
TODO
I'm not sure what this does but that default-autnenticationplugin stuff might be useful - I think it was just for
# alternate suggestion from drupal forum:
# docker run --name mysql8 -p 127.0.0.1:3307:3306 -e MYSQL_DATABASE=drupal -e MYSQL_ROOT_PASSWORD=dev -d mysql:latest --default-authentication-plugin=mysql_native_password
```

Run MySQL client to check your sever is working:

```bash
# mysql client will try to use a unix socket if you use host of 'localhost'
$ mysql -u root -h 127.0.0.1  -p
# supply the root password you passed in to docker above
```

## Step: Rup php-fpm

```sh
cd path/to/project

# Run php-fpm in foreground (logging to stderr). This means we don't have to
# care about the location of the php-fpm log
$ php-fpm -F

# Aside: phpenv puts php-fpm config under:
#       ~/.phpenv/versions/$VERSION/etc/php-fpm.conf
#
# phpenv puts php-fpm logs under:
#       ~/.phpenv/versions/7.3.8/var/log/php-fpm.log
```

## Step: Set up nginx

TODO: can I make nginx log everything to stdout?

```bash
# Copy the template nginx config to the project root
cp templates/nginx.config /path/to/project/nginx.gitignored.conf

# It is important to run nginx with its CWD set to your project root (so that log files are written to ./log in your project etc.)
$ cd path/to/project

$ nginx -p . -c ./nginx.gitignored.conf

# tail the nginx log
$ tail -f log/access.log
```

## Step: Install Drupal 8

```bash
$ cd path/to/app/repo

$ composer install

# import dump.sql into your drupal_app database
$ mysql -u root -h 127.0.0.1 -p -D drupal_app < ./dump.sql

# Set up JS
$ npm i # or yarn install as required
```

### Create settings.php

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

### Setup .envrc

```bash
export AWS_ACCESS_KEY_ID=""
export AWS_SECRET_ACCESS_KEY=""
export PATH=./vendor/bin:$PATH
```

### Set up your admin user

```bash
drush user-password user+1@localhost.localdomain --password="whatever"
```

## Background

### Install a new PHP version

Options:

1. phpenv
    * see instructions below
    * gives most control of versions but building requires wrangling fiddly C deps at build time
2. homebrew
    * has most recent stable version of PHP in the `php` package
    * lives in `/usr/local/bin/php`
3. macOS built-in
   * currently 7.3.9 but will probably never be updated
   * lives in `/usr/bin/php`

#### Step 1: Install PHP on macOS 10.15 (catalina) using phpenv

```bash
# 10.15 instructions
# This **may** be required - I'm not sure because I had already done it before I tried to build any PHP
xcode-select --install

brew install autoconf bison bzip2 curl icu4c libedit libjpeg libiconv libpng libxml2 libzip openssl re2c tidy-html5 zlib

# adjust the PHP version you install here to taste
PATH="$(brew --prefix icu4c)/bin:$(brew --prefix icu4c)/sbin:$(brew --prefix libiconv)/bin:$(brew --prefix curl)/bin:$(brew --prefix libxml2)/bin:$(brew --prefix bzip2)/bin:$(brew --prefix bison)/bin:$PATH" \
PHP_BUILD_CONFIGURE_OPTS="--with-zlib-dir=$(brew --prefix zlib) --with-bz2=$(brew --prefix bzip2) --with-curl=$(brew --prefix curl) --with-iconv=$(brew --prefix libiconv) --with-libedit=$(brew --prefix libedit)" \
phpenv install 7.3.8
```

#### Step 2: Set up VSCode XDebug integration

Add the chunk below to your php.ini via `phpenv configure` (opens EDITOR with the current php.ini)

```ini
[XDebug]
xdebug.remote_enable = 1
xdebug.remote_autostart = 1

# Now set the memory limit to something big (PHPCS requires a lot of memory)
memory_limit = 1024M
```
