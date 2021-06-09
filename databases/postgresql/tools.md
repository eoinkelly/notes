# Tools for SQL

## Formatters

* pgFormatter
    * Perl script which formats SQL - exampe: https://sqlformat.darold.net/
    * Seems to make fairly sensible choices
    * `brew install pgformatter`

## linters

* https://github.com/sqlfluff/sqlfluff
* `pip install sqlfluff`
* -- it can't handle placeholders like `$1`
* -- it cannot handle `SQL COPY () TO STDOUT`
* still bit bugging as of Jun 2021