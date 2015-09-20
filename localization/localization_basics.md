# Localization of software

"Locale support" is an application respecting cultural preferences regarding

* alphabets
* sorting
* number formatting

Operating systems have "locale settings" that an application can read to decide
how it should localize.


## Locale in shell scripts

* Adjust current session's locale via `LANG` env var

    export LANG=en_US.utf8

## How locale info is used in C

* http://en.cppreference.com/w/c/locale/setlocale
* During program startup, the equivalent of `setlocale(LC_ALL, "C");` is
  executed before any user code is run.
* Setting `LC_ALL` is the way to set the whole locale in one command

C stdlib provides two functions

1. setlocale()
1. localeconv()

```c
    #include <locale.h>
    // contains data types, functions, macros for working with locales

    char * setlocale(int category, const char *locale);
```

* The setlocale() function sets the C library's notion of natural language and
  formatting style for particular sets of routines
* it is the only function that can change locale.

Arguments to `setlocale()`

1. The first arg is a macro. It is one of
    * LC_COLLATE
        * changes behaviour of how the string comparison functions:
            * strcoll(3)
            * strxfrm(3)
    * LC_CTYPE
        * changes how character handling and classification work e.g.
            * isupper(3)
            * toupper(3)
            * mblen(3)
            * wctomb(3)
    * LC_MONETARY
        * changes what localeconv(3) returns which describes how numbers are
        printed e.g. with , and/or .
        * changes behaviour of
            * strfmon(3)
    * LC_MESSAGES
        * changes language tha messages are displayed in
        * changes what a positive and negative answer looks like
        * used by functions
            * gettext(3)
            * ngettext(3)
            * rpmatch(3)
        * gettext functions also obey the `LANGUAGE` env var if it is available
    * LC_NUMERIC
        * changes behaviour of
            * printf(3)
            * scanf(3)
    * LC_TIME
        * changes behaviour of
            * strftime(3) // display local time
    * LC_ALL
        * changes all of the above
2. The second arg to setlocale() sets the default locale. If it is "" then the
   locale is set according to the environment using this algorithm
    1. use env var LC_ALL if it exists and is not null
    2. If env var with same name as one of the categories exists and is not null then use that
    value as locale for that category
    3. If env var LANG exists and is not null use it as locale


Only three locales are defined by default:

1. "" => use native environment
2. "C" => use C language environment
3. "POSIX" => use C language environment

Linux has some extra categories (not on mac) - this is from output of `locale`

1. `LANGUAGE`
3. `LC_PAPER`
4. `LC_NAME`
5. `LC_ADDRESS`
6. `LC_TELEPHONE`
7. `LC_MEASUREMENT`
8. `LC_IDENTIFICATION`

Useful locale files on linux

* /usr/lib/locale/
    * where locale info is stored on linux
* /usr/lib/locale/C.UTF-8/
    * contains a bunch of small `LC_*` files containing binary data
* /usr/share/i18n/
    * contains a dir for each lang/locale/???
    * each dir contains a bunch of .mo files
* /usr/share/i18n/SUPPORTED
    * list of supported values and their recommended encoding for the locale name
    ```
    en_NZ.UTF-8 UTF-8
    en_NZ ISO-8859-1
    ```
