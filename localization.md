# Localization of software

"Locale support" is an application respecting cultural preferences regarding

* alphabets
* sorting
* number formatting

Operating systems have "locale settings" that an application can read to decide
how it should localize.

Locale settings are divided into "categories"

Categories on both mac and linux (output of `locale` command)

1. `LANG`
2. `LC_COLLATE`
3. `LC_CTYPE`
4. `LC_MESSAGES`
5. `LC_MONETARY`
6. `LC_NUMERIC`
7. `LC_TIME`
8. `LC_ALL`

Categories linux only (output of `locale` command)

1. `LANGUAGE`
2. `LC_MESSAGES`
3. `LC_PAPER`
4. `LC_NAME`
5. `LC_ADDRESS`
6. `LC_TELEPHONE`
7. `LC_MEASUREMENT`
8. `LC_IDENTIFICATION`

These environment variables affect each locale categories for all locale-aware
programs:

    LC_CTYPE Character classification and case conversion.
    LC_COLLATE Collation order.
    LC_TIME Date and time formats.
    LC_NUMERIC Non-monetary numeric formats.
    LC_MONETARY Monetary formats.
    LC_MESSAGES Formats of informative and diagnostic messages and interactive responses.
    LC_PAPER Paper size.
    LC_NAME Name formats.
    LC_ADDRESS Address formats and location information.
    LC_TELEPHONE Telephone number formats.
    LC_MEASUREMENT Measurement units (Metric or Other).
    LC_IDENTIFICATION Metadata about the locale information.

but again the only thing I can find in `env` is `LC_CTYPE=en_NZ.UTF-8`

    QUESTION: why aren't all locale setting in the shell environment?

##  mac

    locale -a # will provide a list of available locales
    # ... long list of locales ...

    $ locale # show current locale settings
    LANG="en_NZ.UTF-8"
    LC_COLLATE="en_NZ.UTF-8"
    LC_CTYPE="en_NZ.UTF-8"
    LC_MESSAGES="en_NZ.UTF-8"
    LC_MONETARY="en_NZ.UTF-8"
    LC_NUMERIC="en_NZ.UTF-8"
    LC_TIME="en_NZ.UTF-8"
    LC_ALL=

* oddly only `LC_CTYPE` seems to be in my shell `env`

## linux

    sudo locale-gen en_NZ.UTF-8
    # english as spoken in NZ and encoded with UTF-8 rules
    locale -a # will provide a list of available locales

    $ locale # show current locale settings
    LANG=en_US.UTF-8
    LANGUAGE=
    LC_CTYPE=en_NZ.UTF-8
    LC_NUMERIC="en_US.UTF-8"
    LC_TIME="en_US.UTF-8"
    LC_COLLATE="en_US.UTF-8"
    LC_MONETARY="en_US.UTF-8"
    LC_MESSAGES="en_US.UTF-8"
    LC_PAPER="en_US.UTF-8"
    LC_NAME="en_US.UTF-8"
    LC_ADDRESS="en_US.UTF-8"
    LC_TELEPHONE="en_US.UTF-8"
    LC_MEASUREMENT="en_US.UTF-8"
    LC_IDENTIFICATION="en_US.UTF-8"
    LC_ALL=

## windows

???

# All about representing text as numbers

## Character set

* A character set is the set of glyphs (symbols) _and_ the encodings

    character set = {glyphs} + {encoding}
:
## Collations

A collation is a set of rules for comparing characters in a character set. It
is used for sorting and doing string comparisons e.g. less than, equal, greater
than etc.

Examples

* binary
    * compare characters based on the numbers they are encoded as
    * very simplistic
* case-insensitve
    * treat upper and lower case letters as equal

A character set can have 1-many collations

Encoding names have a pattern:

    <character set name>_<language_name>_<suffix>

    where <suffix> can be:

    cs => case sensitive
    ci => case insensitive
    bin => binary

## Unicode

* Unicode assigns each glyph a "code point" which is a positive integer
* Each code point needs to be represented as a binary number - _how_ that
  happens is specified by the "encoding"
* UTF-8 is an _encoding_ i.e. a way of turning a list of codepoints into bytes
* UTF-8 uses 1 byte for a codepoint if it can and 2 bytes if necessary
    * => it is a variable length encoding


The Unicode collation algorithm (UCA) is an algorithm defined in Unicode Technical Report #10, which defines a customizable method to compare two strings

 Default Unicode Collation Element Table (DUCET). This datafile specifies the default collation ordering. The DUCET is customizable for different languages. Some such customisations can be found in Common Locale Data Repository (CLDR).

## Databases and strings

##  MySQL

* very flexible: lets you mix and match character set and collation at
    server, db, table, column levels

## Postgres

http://www.postgresql.org/docs/9.4/static/charset.html

> PostgreSQL uses the standard ISO C and POSIX locale facilities provided by
> the server operating system


# Operating systems
> PostgreSQL uses the standard ISO C and POSIX locale facilities provided by the server operating system


* OS has a "locale setting(s)"
* Postgres `initdb` will use the OS "locale setting" unless you tell it otherwise

```
# you can specify locale at database create time
initdb --locale=sv_SE
```

* format of a locale setting is `<language>_<country>` and is read as
  `<language> as it is spoken in <country>`

> If more than one character set can be used for a locale then the
> specifications can take the form language_territory.codeset. For example,
> fr_BE.UTF-8 represents the French language (fr) as spoken in Belgium (BE),
> with a UTF-8 character set encoding.


LC_COLLATE and LC_CTYPE settings of a database cannot be changed after its creation

> Conceptually, every expression of a collatable data type has a collation.
> (The built-in collatable data types are text, varchar, and char. User-defined
> base types can also be marked collatable, and of course a domain over a
> collatable data type is collatable.)


> The collation of an expression can be the "default" collation, which means
> the locale settings defined for the database. It is also possible for an
> expression's collation to be indeterminate. In such cases, ordering
> operations and other operations that need to know the collation will fail.

> In addition to comparison operators, collations are taken into account by functions that convert between lower and upper case letters, such as lower, upper, and initcap; by pattern matching operators; and by to_char and related functions.


```sql
CREATE TABLE test1 (
    a text COLLATE "de_DE",
    b text COLLATE "es_ES",
    ...
);

-- use implicit collation of the column
SELECT a < 'foo' FROM test1;

-- override implicit collation with an explicit one
SELECT a < ('foo' COLLATE "fr_FR") FROM test1;

-- parser does not know which collation to apply so this is an error
SELECT a < b FROM test1;

-- can do something like
SELECT a < b COLLATE "de_DE" FROM test1;
-- to fix it
```

A collation is a "SQL schema object" that is a map

    SQL name => OS Locales

    LC_COLLATE
    LC_TYPE

* A collation is tied to a character set encoding
    * the same collation name may exist in multiple character sets
* There are 3 collations available on all platforms
    1. `default`
    2. `C`
    3. `POSIX`
* C and POSIX have identical behaviours - they both specify "traditional C" behaviour
    * only uppercase A to Z considered "letters"
    * sorting done strictly by character code byte value
    * postgres considers them different collations so you can't compare a a
      column with a "C" locale and a column with a "POSIX" locale.
