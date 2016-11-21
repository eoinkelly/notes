# The web application Hacker's Handbook 2nd edition

http://mdsec.net/

Note: this was published 2011 (5yrs old now)

## Chap 1

* there are very few web "sites" these days, most are applications
* web applications are used for both internal and external users
* they are often hooked up to business critical systems e.g. data stores

Users don't have to use your client-side code - they can easily script their own inputs

* can deliver input in unexpected order
* can deleiver malicious input
* can omit expected input
* can make many requests in a short period of time

If an org deploys web apps (especially in the cloud) then the "security
perimeter" extends to the code that runs in the browser of every user, not just
what is outside the corporate firewall.

There are two targets of web app vulnerabilities:

1. The owner of the web app
    * defects in the app allows an attacker to gain information or access to the owner's system
2. Other users of the web app
    * defects in the app allows it to become the vehicle for delivery of an attack against another user of the same app

Interesting idea: New technology brings 1) new attacks and 2) new variations on existing attacks

Common vulnerability categories

1. Broken authentication
2. Broken access controls
3. SQL injection
4. Cross site scripting
    * attacks other users on the site not the site owner
5. Information leakage
6. Cross site request forgery
    * attacks other users on the site not the site owner

From the stats shown in this chapter it seems SQLi is much less common than the
other types in the above list.

**The core problem is that applications allow arbitrary user input - every input
from the user should be considered malicious.**

END CHAP 1

## Chap 2

Every web app has four core "mechanisms" that it uses for security:

1. Handling user access
    * Most apps handle user access with the following three "mechanisms"
        1. Authentication - prevent unauthorized access
            * as well as the basic login/logout they usually have a collection of related functionality which provides attack surface
                * password recovery
                * editing user details incl. password
                * self registration
        1. Session management
            * usually managed by creating a session for the user on the server and putting a token that identifies the session to the user, often in a cookie but can be hidden form field
            * if a user does not make a request for a certain amount of time their session is expired
            * is often attacked by trying to get or guess the tokens of other users
        1. Access control
            * enforces whether a given user can access certain resources
            * can contain complex logic so devs can make logic errors which can be exploited
    * These 3 mechanisms are linked and are only as strong as the weakest link in the chain
2. handling user input
    * prevent malformed input from causing problems
    * hard to do
    * sometimes apps have to accept arbitrary user input in a field
    * ways app can get input (and all of these can be malicious)
        * GET request URL and params
        * POST request URL and body
        * HTTP Headers
            * Cookie
    * Approaches to input handling
        1. Reject known bad (aka Blacklist)
            * least effective method because the parser used by the blacklist
              can be fooled by malicious input being represented or encoded in
              different ways e.g.
                * `SELECT` can be `SeLeCt` and ` OR 1=1` can be ` OR 2=2` etc.
                * can put non standard characters between expressions `SELECT\*blah*\username,password\*foo*\FROM users;`
                * insert a null byte to make parser think string has ended e.g. `%00<script>alert('stuff');</script>`
        2. Accept known good (aka Whitelist)
            * When it is feasible it is the best way to handle potentially malicious input
            * Can be implemented by mapping the user input to known good output so the user input is not used beyond the validation
            * Problem with whitelisting is that you can't use it much of the time because the "known good" input is not well constrained e.g. a person's name.
        3. Sanitization
            * Accepts that sometimes data can't be garuanteed to be safe
            * This approach is widely applicable and effective
            * Tries to clean the data so that unsafe bits are removed or made harmless
            * Approaches
                1. remove unsafe characters
                2. re-encode or "escape" unsafe characters in the input
            * Examples
                * Prevent XSS by HTML encoding dangerous characters before they
                  are embedded into a web page
        4. Safe data handling
            * Don't process potentially malicious input data in unsafe ways
            * This approach is widely applicable and effective
            * Examples
                * SQLi can be mitigated by using parameterized queries or
                  prepared statements - the input data is still just as
                  dangerous but we don't process it in an unsafe way
        5. Semantic checks
            * Sometimes malicious input is indistinguisable from good input
              e.g. if attacker submits a different bank account for a transfer
              to go to.
            * We can defend against this by verifying the meaning of the input
              before processing e.g. does this account belong to this person
              etc.
    * Boundaries
        * it is tempting to think of validation as the wall between "bad input
          from outside" and "good input I can 100% trust". This is a bad idea
          because
            * it is too hard to validate for all possible malicious input in
              all ways in one place in the code e.g. semantic validation
            * data is transformed as it is processed. The attacker might have
              crafted the input so that it seems benign at the boundary wall
              but will become malicious **after** it has been transformed by
              some internal processs
            * defending against all kinds of attacks in one place can be
              impossible e.g. defend gainst XSS by changing `>` to `&gt;` but
              then preventing command injection attacks will want to prevent
              use of `&` and `;`.
        * The solution is "defense in depth" or "boundary validation"
            * each component in the code treats the input as potentially malicious
            * each component defends itself against the specific kind of
              malicious input that it is vulnerable to
    * Multistep validation
        * If app removes `<script>` the attacker can supply `<scr<script>ipt>`
        * data needs to be sanitized recursively
        * attacker can exploit the order that filters are applied to craft
          input where the filtering itself is what creates the malicious output
    * Canonicalization
        * the process of converting or decoding data into a common character set
        * if Canonicalization is applied **after** filtering the attacker can
          use a suitable encoding scheme to hide malicious input from filters
        * QUESTION: what does rails do about input canonicalization?
        * e.g. `%2527` gets URL decoded to `%27`
3. Handling attackers
    * assume your app will be targetted by dedicated and skilled attackers
    * the app needs mechanisms to respond to attack with defensive AND offensive measures e.g.
        1. handling errors
            * handle unexpected errors gracefully - never return system
              generated error messages as attackers can trigger them such that
              they reveal information e.g. ask for `user` in a SQL query and if
              error message tells you that there was a type mismatch then it
              might also dump the user as a string to help debug it.
        2. Maintain audit logs
            * lets you investigate intrusion attempts and find out
                * when the compromise happened
                * how much data they accessed
                * the credentials that were used to access the data
            * things which should be logged include
                * authentication events
                    * user login/logout
                    * password change
                * money events
                    * took payment
                    * refund issued
            * audit logs should be stored on a separate system to the one being protected
                * audit log system only accepts updates from the system, no deletes
            * audit logs have themselves to be well protected - if compromised the attacker can see the whole system
        3. alerting admins
            * ideally the app will alert and admin when an attack is under way so admin can take steps to mitigate it e.g.
                * block attacking IP address
                * take app offline
            * examples of things to alert on
                * usage anomalies
                    * large numbers of requests from a single IP or user
                * business anomalies
                    * funds going somewhere unexpected
                * requests containing known attack strings
                * requests where data which is hidden from ordinary users has been modified e.g. cookie
            * off the shelf _web application firewalls_ are good at doing the basics of this but because each app is different they won't necessairly catch a subtle attack
            * the most effective way to implement real-time alerting is to integrate with the application validations and input sanitizing - validations can throw alerts if they fail in ways which are anomalous
        4. reacting to attacks
            * app can react defensively if it detects anolmaous behaviour e.g.
                * slow down the attackers requests
                * terminate the attacking session
            * these measures will likely not defeat an attacker but they can slow them down and buy admins some time
4. Managing the application
    * enable admins to monitor it
    * enable admins to configure it
    * provides a lot of attack surface for attackers - any vulnerabilities in the admin interface provide potentially much greater access than those in the app itself
        * attacker may be able to
            * create new admin accounts
            * execute XSS against other admins or users
            * admins are often more trusted their access can lead to compromising the box that the app runs on

All thinking about attack and defense is grouped under these headings

Each mechanism listed above may have flaws so is included in the attack surface

END CHAP 2

# Chap 3

* HTTP is stateless
* `Host` header is mandatory in HTTP 1.1
* Cookies
    * Appear as `Set-Cookie` header in server response
    * Appear as `Cookie` header in client request

HTTP verbs

GET
POST
PUT
PATCH
HEAD
OPTIONS
    * asks HTTP server to report which methods are available for a particular resource
TRACE
