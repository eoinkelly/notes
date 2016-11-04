# Managing tainted input

Input scrubbing is hard to get right
    * they can encode as hex in UTF-8 or other text encoding

Techniques for input scrubbing

1. Whitelisting
    * Map the input to some safe input using a case statement or Hash lookup
    * ++ input from user never makes it past the whitelist lookup
    * -- only works when the no. of valid inputs is very constrained
1. Escaping
    * -- frail
    * -- usually DB specific
1. Escaping by hex encoding
    * you hex encode all strings before sending them to the DB. Your query has to wrap them in a `hex_decode()` function of some sort.
    * a sub case of escaping
    * -- still frail
    * ++ the string you send contains only 0-9a-f
    * -- This does not always work
        * WHY?
    * -- does not protect you from second order SQLi
        * it may prevent DB from running the user supplied content as SQL this time but it will still be stored in DB


Techniques for avoiding input scrubbing

* avoid blacklisting of certain character sequences
    * `SELECT` can be `SeLeCt` and ` OR 1=1` can be ` OR 2=2` etc.
    * can put non standard characters between expressions `SELECT\*blah*\username,password\*foo*\FROM users;`
    * insert a null byte to make parser think string has ended e.g. `%00<script>alert('stuff');</script>`

