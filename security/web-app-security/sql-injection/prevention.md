# SQLi prevention

https://www.owasp.org/index.php/SQL_Injection_Prevention_Cheat_Sheet

6 techniques

1. Always use prepared statements
    * this is the most important technique
2. Have your dynamically built SQL string call a stored procedure rather than
   doing the query direclty
    * using stored procedures is not in itself inheriently safe e.g. `'CALL myProcedure(' + $userInput + ')'` is problematic
        * TODO: refresh on postgres stored procedure syntax
    * **but** most DBs have a stored procedure syntax that allows you to
      parameterize the arguments (much like prepared statements do for normal
      SQL) and this parameterization can provide the same safeguards that
      prepared statements do.
3. Whitelist input validation
    * don't use the input direclty in the string - use the input to find the
      string that should be used in the query e.g. if you need a dynamic table
      name then don't have the input be the table name, have it be a key that
      can be used to find the table name form a `case` statement or a hash or
      some other means
        * another example is sort order is a string which can be used to lookup a boolean
    * the important bit is that the input never gets sent to the DB
    * this is a good defense to add with prepared statements to get better protection
4. Escaping all user supplied input
    * this is frail compared to the methods above
    * any of the techniques above are better than this
    * string escaping for the DB is usually DB specific
    * a specific case of this is where you hex encode all strings before sending them to the DB. Your query has to wrap them in a `hex_decode()` function of some sort.
        * this has the advantage of knowing for sure that the string you send contains only5-9a-f
    * This does not always work
6. Least privilege
    * Your webapp should use a DB account with the least privilege possible
    * It should not be able to create users i.e. don't run as an admin
    * the DB process should run with the least privileges possible from the OS
    * have multiple DB users so that if your app gets popped then they don't have access to all tables
    * you can use views + permissions to give a DB user an even more restricted access to the DB e.g. they might only have read access to certain views which are automatically built by stored procedures (but they don't have access to change the stored procedures)

## Postgres

* TODO: refresh on postgres roles and permissions model
* postgres creds are system creds by default - does this mean postgres doesn't have a "users" table?

## How prepared statements help

the query and the data are sent to the SQL server separately.

 SQL query is a full legitimate program. And we are creating this program dynamically by adding some data on the fly.

 The root of the SQL injection problem is mixing of the code and the data.

 bound parameters will help with the primary SQLi i.e. if you submit `'; DROP tables;--` and the query uses bound params then it won't be run **but** if other code picks up that string later on and runs it naievely then it will work. That other code can be
    * code in the app
    * DB trigger, stored procedure, user defined function

> With bound parameters, the query tree is parsed, then, in PostgreSQL at least, the data is looked at in order to plan. The plan is executed. With prepared statements, the plan is saved so you can re-execute the same plan with different data over and over (this may or may not be what you want). But the point is that with bound parameters, a parameter cannot inject anything into the parse tree. So this class of SQL injection issue is properly taken care of.


