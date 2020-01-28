# Installing PHP on macOS

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

## Install PHP on macOS 10.15 (catalina) using phpenv

```bash
# 10.15 instructions
# This **may** be required - I'm not sure because I had already done it before I tried to build any PHP
xcode-select --install

brew install autoconf bison bzip2 curl icu4c libedit libjpeg libiconv libpng libxml2 libzip openssl re2c tidy-html5 zlib

PATH="$(brew --prefix icu4c)/bin:$(brew --prefix icu4c)/sbin:$(brew --prefix libiconv)/bin:$(brew --prefix curl)/bin:$(brew --prefix libxml2)/bin:$(brew --prefix bzip2)/bin:$(brew --prefix bison)/bin:$PATH" \
PHP_BUILD_CONFIGURE_OPTS="--with-zlib-dir=$(brew --prefix zlib) --with-bz2=$(brew --prefix bzip2) --with-curl=$(brew --prefix curl) --with-iconv=$(brew --prefix libiconv) --with-libedit=$(brew --prefix libedit)" \
phpenv install 7.3.8


# dump the config used to build whatever the current version of php is
php-config
```

## Setting up VSCode XDebug integration

Add the chunk below to your php.ini via `phpenv configure` (opens EDITOR with the current php.ini)

```ini
[XDebug]
xdebug.remote_enable = 1
xdebug.remote_autostart = 1
```