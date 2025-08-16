```bash
cd path/to/my/code

## Download Drupal
$ curl -O https://ftp.drupal.org/files/projects/drupal-7.67.tar.gz

# alternate suggestion from drupal forum:
# docker run --name mysql8 -p 127.0.0.1:3307:3306 -e MYSQL_DATABASE=drupal -e MYSQL_ROOT_PASSWORD=dev -d mysql:latest --default-authentication-plugin=mysql_native_password

# WARNING: Drupal 7 does not support MySQL 8
docker run --name mysql --detach -p "3306:3306" -e MYSQL_ROOT_PASSWORD=password  mysql:5.7

# docker run --name mysql --restart=always -e MYSQL_ROOT_PASSWORD=my-secret-pw -d mysql:latest

# mysql will try to use a unix socket if you use host of 'localhost'
$ mysql -u root -h 127.0.0.1  -p

# these were required on mysql 8 but not 5.7
# CREATE USER 'root'@'%' IDENTIFIED BY 'password';
# GRANT ALL PRIVILEGES ON *.* TO 'root'@'%';
# ALTER USER 'root' IDENTIFIED WITH mysql_native_password BY 'password';

# this is required
CREATE DATABASE drupal_attempt_1 CHARACTER SET utf8 COLLATE utf8_general_ci;

# setting up apache
# option 1: docker
# $ docker run -dit --name eoin-apache -p 8080:80 -v "$PWD":/usr/local/apache2/htdocs/ httpd:2.4
# doesn't work because no php-fpm in that yet

# option 2: homebrew + phpenv
# step 1: install apache
brew install apache-httpd

# step 2: edit apache config file
vim /usr/local/etc/httpd/httpd.conf

sudo apachectl restart
# cd to the project you care about so that we pick-up .php-version file when we run php-fpm
cd path/to/drupal/project


# run php-fpm in foreground (logging to stderr)
# this means we don't have to care about the location of the php-fpm log
php-fpm -F

# phpenv puts php-fpm config under:
# ~/.phpenv/versions/$VERSION/etc/php-fpm.conf
# phpenv puts php-fpm logs under:
# ~/.phpenv/versions/7.3.8/var/log/php-fpm.log
# note: logs don't matter if you use run it in foreground with -F

# Aside: apache log files are at:
# less /usr/local/var/log/httpd/access_log
# less /usr/local/var/log/httpd/error_log

# Aside: apache web root:
# /usr/local/var/www
```

there is a system apache conf and my custom conf - ideally join with one line
there is a built-in php-fpm conf for each php version but I do want to customise
it I want them to generate log files where I can see them and log verbosely so i
can see things go wrong

```php
<?php
// Use PDO to directly connect to DB - diagnoses issues more easily than the drupal install errors
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

$dsn = "mysql:host=127.0.0.1;dbname=drupal_attempt_1";
$user = "root";
$passwd = "password";

$pdo = new PDO($dsn, $user, $passwd);

$stm = $pdo->query("SELECT VERSION()");

$version = $stm->fetch();

echo $version[0] . PHP_EOL;

```

## Aside: Setting up PHP for local dev

```bash
# open php.ini for the local php
$ phpenv configure

# change memory_limit to something big (phpcs requires a lot)
# turn on error reporting output etc.
```

## Setup drupal coding standards

- https://git.drupalcode.org/project/coder

```bash
# install composer module
composer global require drupal/coder

# show installed composer modules and their paths
composer global show -P

# show avaiilable rulesets for phpcs
phpcs -i

# add the Drupal, DrupalBestPractice rulesets from coder to phpcs
ls -l ~/.composer/vendor/drupal/coder/coder_sniffer
phpcs --config-set installed_paths ~/.composer/vendor/drupal/coder/coder_sniffer
phpcs -i

# run codesniffer
cd path/to/drupal/project
phpcs --standard=Drupal .
phpcs --standard=DrupalPractice .
phpcs -d memory_limit=1024M --standard=Drupal . # increase memory limit if you run out of memory
```
