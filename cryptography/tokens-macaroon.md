## Macaroons

* https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/41892.pdf
* Paper by Google Research, there is no full spec
* Auth token format
* only one crypto algo used
* extensable in that you can add additional constraints to a token before using it or passing it to another service
    * allows you to
        * bind the token to a single request by including a hash of the requst or pin the TLS connection
* claims are called "caveats"
    * i.e. you are adding restrictions from a default "god mode" token
    * you can only add caveats, not remove
    * additional caveats assed as a chain of HMAC, each one depending on the previous HMAC
* can use AED to link tokens together e.g. this token lets you do X provided the other token asserts you are Y
* not commonly used
* Some folk thinks the open source libs aren't good and it's a lot of work to implement
* Conclusion: not easily usable in production today
