# PHP

## Building on macOS using phpenv

```
$ brew install openssl

# Install phpenv and phpenv-install as per instructions on their githubs

# required for phpenv to find brew's version of openssl
$ export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig"

# required to get zlib headers (at least, maybe other headers too) in a place where phpenv can find them
$ open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg

$ phpenv install 7.2.9

# not all versions of php require all these packages but I needed all these to install 7.1 -> 7.3
$ brew install re2c libzip libmcrypt

$ phpenv install 7.3.8
```

```
# Building PHP via phpenv defaults creates the following binaries:

~/.phpenv/versions/7.2.9/bin $ ls -l
lrwxr-xr-x 1 eoinkelly staff        9 Aug 24 09:10 phar -> phar.phar
-rwxr-xr-x 1 eoinkelly staff    14858 Aug 24 09:10 phar.phar
-rwxr-xr-x 1 eoinkelly staff 17250416 Aug 24 09:10 php
-rwxr-xr-x 1 eoinkelly staff 17188480 Aug 24 09:10 php-cgi
-rwxr-xr-x 1 eoinkelly staff     3836 Aug 24 09:10 php-config
-rwxr-xr-x 1 eoinkelly staff 17426116 Aug 24 09:10 phpdbg
-rwxr-xr-x 1 eoinkelly staff     4645 Aug 24 09:10 phpize


# dump the config used to build php

$ php-config
Usage: /Users/eoinkelly/.phpenv/versions/7.3.8/bin/php-config [OPTION]
Options:
  --prefix            [/Users/eoinkelly/.phpenv/versions/7.3.8]
  --includes          [-I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php -I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php/main -I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php/TSRM -I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php/Zend -I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php/ext -I/Users/eoinkelly/.phpenv/versions/7.3.8/include/php/ext/date/lib]
  --ldflags           [ -L/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk/usr/lib -L/usr/local/Cellar/openssl/1.0.2s/lib -L/usr/local/lib -L/usr/local/Cellar/icu4c/64.2/lib -L/usr/local/Cellar/libzip/1.5.2/lib]
  --libs              [  -lzip -lzip -lz -lexslt -ltidy -lresolv -ledit -lncurses -lstdc++ -liconv -liconv -lpng -lz -ljpeg -lbz2 -lz -lm  -lxml2 -lz -licucore -lm -lkrb5 -lssl -lcrypto -lcurl -lxml2 -lz -licucore -lm -lssl -lcrypto -licui18n -licuuc -licudata -licuio -lxml2 -lz -licucore -lm -lxml2 -lz -licucore -lm -lxml2 -lz -licucore -lm -lxml2 -lz -licucore -lm -lxml2 -lz -licucore -lm -lxml2 -lz -licucore -lm -lxslt -lxml2 -lz -licucore -lm -lssl -lcrypto ]
  --extension-dir     [/Users/eoinkelly/.phpenv/versions/7.3.8/lib/php/extensions/no-debug-non-zts-20180731]
  --include-dir       [/Users/eoinkelly/.phpenv/versions/7.3.8/include/php]
  --man-dir           [/Users/eoinkelly/.phpenv/versions/7.3.8/share/man]
  --php-binary        [/Users/eoinkelly/.phpenv/versions/7.3.8/bin/php]
  --php-sapis         [ cli fpm phpdbg cgi]
  --configure-options [--with-config-file-path=/Users/eoinkelly/.phpenv/versions/7.3.8/etc --with-config-file-scan-dir=/Users/eoinkelly/.phpenv/versions/7.3.8/etc/conf.d --prefix=/Users/eoinkelly/.phpenv/versions/7.3.8 --libexecdir=/Users/eoinkelly/.phpenv/versions/7.3.8/libexec --datadir=/Users/eoinkelly/.phpenv/versions/7.3.8/share/php --mandir=/Users/eoinkelly/.phpenv/versions/7.3.8/share/man --without-pear --with-gd --enable-sockets --with-jpeg-dir=/usr --enable-exif --enable-zip --with-zlib --with-zlib-dir=/usr --with-bz2 --enable-intl --with-kerberos --with-openssl --enable-soap --enable-xmlreader --with-xsl --enable-ftp --enable-cgi --with-curl=/usr --with-tidy --with-xmlrpc --enable-sysvsem --enable-sysvshm --enable-shmop --with-mysqli=mysqlnd --with-pdo-mysql=mysqlnd --with-pdo-sqlite --enable-pcntl --enable-mbstring --disable-debug --enable-fpm --enable-bcmath --enable-phpdbg --with-libedit --with-png-dir=/usr/X11 --with-icu-dir=/usr/local/opt/icu4c]
  --version           [7.3.8]
  --vernum            [70308]
```

## Features

* Interactive shell:
    * Run `php -a`
    * Has tab completion
    * doesn't echo expressions by default :-( - use `echo SOME_EXPRESSION;` to see output
* Built-in web server
    * example
        ```
        $ cd /path/to/webroot
        $ php -S localhost:3001

        # or
        $ php -S localhost:3001 -t /path/to/webroot
        ```
    * Don't use for production envs
    * Is quite slow
        * The web server runs only one single-threaded process, so PHP applications will stall if a request is blocked.


## Running PHP

* PHP has a "I get input from a web server" view of the world and they use that for CLI too - the CLI gets treated as a particular kind of "web server"
* PHP has a number of SAPIs _Server Application Programming Interfaces_
    1. Direct module interface (Question: is this an SAPI too?)
        * Used by some webservers e.g. Apache
        * Runs php as a dynamically loaded module into the web server process
        * Fast but inflexible in some ways (see FPM details below for comparison)
    1. CGI SAPI
        * Used when a webserver invokes PHP via ascii CGI protocol (see notes file on CGI for details)
    1. FastCGI SAPI
        * Used when a webserver invokes PHP via the binary FastCGI protocol (see notes file on CGI for details)
    1. CLI SAPI
        * Used for command line usage
    1. CLI Web Server
    * The SAPI can be introspected by `php_sapi_name()` function or `PHP_SAPI` constant
        * Although not exhaustive, the possible return values include aolserver, apache, apache2filter, apache2handler, caudium, cgi (until PHP 5.3), cgi-fcgi, cli, cli-server, continuity, embed, fpm-fcgi, isapi, litespeed, milter, nsapi, phpdbg, phttpd, pi3web, roxen, thttpd, tux, and webjames.
* When `phpenv` builds PHP it enable these SAPIs: `cli fpm phpdbg cgi`
* Your PHP app can check which SAPI it is running under and take different actions at runtime
    ```php
    <?php
    // router.php
    if (php_sapi_name() == 'cli-server') { // etc.
        /* route static assets and return false */
    }
    /* go on with normal index.php operations */
    ?>
    ```
* Notice the SAPI is given in parens after the version number when you run `php -v`
    ```
    $ php -v
    PHP 7.3.8 (cli) (built: Aug 24 2019 09:30:03) ( NTS )
    Copyright (c) 1997-2018 The PHP Group
    Zend Engine v3.3.8, Copyright (c) 1998-2018 Zend Technologies
        with Zend OPcache v7.3.8, Copyright (c) 1999-2018, by Zend Technologies
        with Xdebug v2.7.2, Copyright (c) 2002-2019, by Derick Rethans

    $ php-fpm -v
    PHP 7.3.8 (fpm-fcgi) (built: Aug 24 2019 09:30:14)
    Copyright (c) 1997-2018 The PHP Group
    Zend Engine v3.3.8, Copyright (c) 1998-2018 Zend Technologies
        with Zend OPcache v7.3.8, Copyright (c) 1999-2018, by Zend Technologies
        with Xdebug v2.7.2, Copyright (c) 2002-2019, by Derick Rethans
    ```

Tools

* `php-config` can be used to introspect the build configuration of your PHP
* `phpize`
    * https://www.php.net/manual/en/install.pecl.phpize.php
    * developer tool used to prepare a shell env for building PECL extensions

Runtime configuration

* All runtime config happens via `php.ini`
* if `php-SAPINAME.ini` exists it will be used **instead of** not as well as `php.ini`
* Search path for `php.ini` (in order)
    1. SAPI module specific location e.g. command line option for CLI or CGI SAPI
    1. `PHPRC` environment variable
    1. Current working directory (except CLI SAPI)
    1. web servers SAPI module directory
* `phpenv` puts the INI files in `/Users/eoinkelly/.phpenv/versions/$VERSION/etc/php.ini`

### PHP-FPM

https://www.php.net/manual/en/install.fpm.php

* is a binary which gets built when you build PHP (it is part of standard PHP distribution)
* is a binary which runs on your system and listens on a unix/tcp socket
* comes with its own systemd service file if you need it
* web servers send "FastCGI" requests over the socket
* it runs (configurable) pools of PHP processes
* it seems to be (conceptually) an exchange of `FCGI_Record` C structs shoved into/out of the the socket
* Differences from Ruby Webrick/Puma/Unicorn etc.
    * Webrick/Puma/Unicorn
        * accept HTTP requests and generate HTTP responsesover the socket they listen on
        * => don't require something to do HTTP sitting in front of them
        * when nginx is in front of puma it is being a HTTP proxy (accepting HTTP from clients and sending HTTP to puma).
    * PHP-FPM expects FastCGI protocol
        * => requires a server that can speak HTTP sitting in front
        * when nginx is in front of php-fpm it is accepting HTTP from clients and sending FastCGI to php-fpm (and reversing that translation on the way back)

```
# PHP-FPM installed by phpenv on macOS

# fpm binary
~/.phpenv/versions/$VERSION/etc/init.d/php-fpm

# config file
~/.phpenv/versions/$VERSION/etc/php-fpm.conf
#
# by default this config tells fpm to
# * listen on tcp://127.0.0.1:9000
# * run as nobody.nobody
```

### PHP CLI

* Uses a CLI SAPI
* `php -v` tells you which SAPI that PHP binary is using
* The CGI and CLI SAPIs are similar but different

```
$ php -v
PHP 7.1.23 (cli) (built: Feb 22 2019 22:19:32) ( NTS )
Copyright (c) 1997-2018 The PHP Group
Zend Engine v3.1.0, Copyright (c) 1998-2018 Zend Technologies
```

