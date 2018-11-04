

Options

* https://github.com/ueberauth/guardian
    * stars: 972
    * born (ish): Jun 2015
    * provides
        * authentication: YES
        * authorization: NO
        * user can edit their details: NO
        * invitable users: NO
        * confirmable users: NO
* https://github.com/hassox/guardian_db
    * works with guardian
    * by default guardian doesn't store tokens anywhere when it generates them for authenticated users
    * guardian_db stores the token in the DB and will check the token provided by the user against the DB each time.
    * it will also remove the token from DB when it is revoked (user logs out)
        * without this, guardian tokens can basically be used for ever
            * -- relies on the browser/client to actually throw it away
* https://github.com/ueberauth/ueberauth
    * is a _framework_
    * allows users to prove their identity to the app
    * does NOT authenticate each request - guardian does this
    * has a bunch of strategies
        * each strategy is a plug
        * strategies for authenticating against variou oauth2 providers e.g. google, facebook
        * the "identity strategy" allows you to authenticate with your own app
* https://github.com/opendrops/passport
    * provides
        * authentication: YES
        * authorization: NO
        * user can edit their details: ???
        * invitable users: ???
        * confirmable users: ???
    * uses comeonin for password hashing
* https://github.com/britton-jb/sentinel
    * is attempting to be devise
    * -- is immature
    * uses guardian and guardian_db, comeonin, bamboo

guardian + ueberauth + ueberauth_identity = an auth solution
    * but is missing confirmable, invitable users
    * also missing user management of their own stuff
