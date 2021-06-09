# SQL Style

General tips

* Never use `SELECT *` in production code because it'll break if you add/remove columns
* Always quote identifers (SQL is case-insensitive)

My faves:

    Gitlab style guide (seems closest to what I like)
    pgFormatter tool (seems the most useful one)

## Style Guides

* https://about.gitlab.com/handbook/business-technology/data-team/platform/sql-style-guide/
    * seems good
    * advocates for not using aliases but explicitly naming the `table.col` each time you reference it (wordy but possibly good?)
* https://www.sqlstyle.guide/
    * seems good but advocates fiddly "rivers" use of whitespace
    * seems to be a foundation for other styleguides
* https://docs.telemetry.mozilla.org/concepts/sql_style.html
    * Based on the sqlstyle.guide one but more pragmatic whitespace
* https://gist.github.com/fredbenenson/7bb92718e19138c20591
    * apparently from kickstarter

## Formatters

* pgFormatter
    * `brew install pgformatter`
    * Perl script which formats SQL - exampe: https://sqlformat.darold.net/
    * Seems to make fairly sensible choices
    * seems to work on complex SQL
    * matches the Gitlab guide for the most part except
        * doesn't add extra empty line before/after CTE queries
        * indents by 4 spaces by default
* https://github.com/sqlfluff/sqlfluff
    * `pip install sqlfluff`
    * -- it can't handle placeholders like `$1`
    * -- it cannot handle `SQL COPY () TO STDOUT`
    * still bit buggy as of Jun 2021

## linters

* `npm install -g sql-lint`
    * actually connects to your DB
    * checks for errors but not style stuff
    * Has quite a small suite of checks