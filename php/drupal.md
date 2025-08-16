# Drupal

## Timeline

- Drupal 9 due June 2020
    - Drupal 7 supported until Nov 2021
- 9 will be identical to the last version of 8 but with deprecations removed and
  3rd party deps updated
    - so if an 8.x is kept up to date with minor versions then the jump to 9
      should be ok
- The drupal team think that 8-9 will be the easiest upgrade yet
  https://www.drupal.org/about/9/get-ready because 9 is not a complete
  re-invention of the platform
- minor releases are released every 6 months
- https://github.com/mglaman/drupal-check can be used to identify deprecations
  in 8.x code
- Drupal 9 will use twig2, 8.x used twig1

- it comes with server config files in multiple directores to control access
    - `.htaccess` for Apache
    - `web.config` for IIS

## Install

```
# macOS instructions

brew install composer
brew tap drud/ddev && brew install ddev

# mkcert -install # if you want to use HTTPS (it was breaking drupal install for me but I'm probably missing something)

# Created a new local CA at "/Users/eoinkelly/Library/Application Support/mkcert" 💥
# The local CA is now installed in the system trust store! ⚡️
# The local CA is now installed in the Firefox trust store (requires browser restart)! 🦊
# The local CA is now installed in Java's trust store! ☕️

composer create-project drupal-composer/drupal-project:8.x-dev --stability dev --no-interaction NAME-OF-PROJECT

cd NAME-OF-PROJECT

git init && git add . && git commit -m "COMPOSER_COMMAND_YOU_RAN_TO_CREATE_PROJECT"

ddev config --docroot web --project-name NAME-OF-PROJECT --project-type drupal8

git add . && git commit -m "YOUR_LAST_COMMAND"

ddev start

git add . && git commit -m "YOUR_LAST_COMMAND"

ddev describe

# cleanup

ddev delete --omit-snapshot
```

- A drupal 8 project is approx 180 MB of code

Hint: use `-` as the separator in project names

why mkcert - is it required or optional?

## ddev

- wraps around docker to automate setting up a dev env with
    1. a web container
        - runs drupal, nginx, php-fpm, mailhog, managed by supervisord
    2. a database container
    3. an ssh container
        - allows you to ssh into any container
    4. a phpmyadmin container
        - TODO: why is this separate container?
    5. a router
        - runs nginx
        - binds to host machine ports, routes connections to those ports to the
          appropriate other container and handles SSL termination for the
          connections too
    6. an optional solr container
- it uses nginx-fpm by default but can use apache-fpm or apache-cgi
- it constructs a docker-compose.yml in the .ddev dir

Commands

    ddev start

Containers

```


CONTAINER ID        IMAGE                                   COMMAND                  CREATED             STATUS                             PORTS                                                                                              NAMES
f18a8b50508d        drud/ddev-router:v1.10.0                "/app/docker-entrypo…"   14 seconds ago      Up 12 seconds (healthy)            127.0.0.1:80->80/tcp, 127.0.0.1:443->443/tcp, 127.0.0.1:8025->8025/tcp, 127.0.0.1:8036->8036/tcp   ddev-router
c39aadf98881        drud/ddev-webserver:v1.10.2-built       "/start.sh"              37 seconds ago      Up 33 seconds (healthy)            8025/tcp, 127.0.0.1:32770->80/tcp, 127.0.0.1:32769->443/tcp                                        ddev-eoin-drupal-test-1-web
95c90ab4ab6b        drud/phpmyadmin:v1.10.0                 "/run.sh phpmyadmin"     37 seconds ago      Up 34 seconds (health: starting)   0.0.0.0:32768->80/tcp                                                                              ddev-eoin-drupal-test-1-dba
ae7db75d2c9e        drud/ddev-dbserver:v1.10.0-10.2-built   "/docker-entrypoint.…"   39 seconds ago      Up 37 seconds (healthy)            127.0.0.1:32768->3306/tcp                                                                          ddev-eoin-drupal-test-1-db
e6e89c0a8818        drud/ddev-ssh-agent:v1.10.2-built       "/entry.sh ssh-agent"    3 minutes ago       Up 3 minutes (healthy)                                                                                                                ddev-ssh-agent


```

DNS fanciness

```
Project can be reached at http://eoin-drupal-test-1.ddev.site http://127.0.0.1:32770
```

`ddev.site` is registered in Google CLoud and will reply with `127.0.0.1` to an
A record request for _any_ subdomain under it e.g. `blah.ddev.site`.

## Command: ddev describe

```
dev describe                                                                                             
NAME                TYPE     LOCATION                                     URL                                  STATUS
eoin-drupal-test-1  drupal8  ~/Code/repos/drupal-play/eoin_drupal_test_1  http://eoin-drupal-test-1.ddev.site  running

Project Information
-------------------
PHP version:   	7.2
MariaDB version	10.2

URLs
----
http://eoin-drupal-test-1.ddev.site
http://127.0.0.1:32770
https://eoin-drupal-test-1.ddev.site
https://127.0.0.1:32769


MySQL/MariaDB Credentials
-------------------------
Username: "db", Password: "db", Default database: "db"

or use root credentials when needed: Username: "root", Password: "root"

Database hostname and port INSIDE container: db:3306
To connect to db server inside container or in project settings files:
mysql --host=db --user=db --password=db --database=db
Database hostname and port from HOST: 127.0.0.1:32768
To connect to mysql from your host machine,
mysql --host=127.0.0.1 --port=32768 --user=db --password=db --database=db


Other Services
--------------
MailHog:   	http://eoin-drupal-test-1.ddev.site:8025
phpMyAdmin:	http://eoin-drupal-test-1.ddev.site:8036

DDEV ROUTER STATUS: healthy
ssh-auth status: healthy
```

"Installing" drupal makes only one change to the project's filesystem (adds a
.htaccess file) - it seems to be mostly "installing" it into the database.

## Drupal development (post installation)

Drupal things:

1. views
1. blocks
    - can be placed within regions in a theme
    - have options that can be configured
1. regions
    - places in the theme HTML where blocks can be inserted
    - every theme provides a different set of regions
1. modules
    - analagous to plugins in wordpress
    - browse available modules at https://www.drupal.org/project/project_module
1. content types
    - examples: page, article
    - presumably you can make your own

## Themes

live in `web/themes/themename` within the project

- seem to have a `themename.theme` PHP file which they kick off from
- mixture of
    - PHP files (_.php, _.inc, \*.theme)
    - JS, CSS, SCSS
    - Twig templates (\*.twig)
    - lots of yaml config files in the `mayo` theme at least

## Tools

- Drupal Console
    - The Drupal CLI. A tool to generate boilerplate code, interact with and
      debug Drupal

## Database abstraction layer

PHP Data Objects

- Built into PHP
- Has drivers for most popular databases

> DO provides a data-access abstraction layer, which means that, regardless of
> which database you're using, you use the same functions to issue queries and
> fetch data. PDO does not provide a database abstraction; it doesn't rewrite
> SQL or emulate missing features.
