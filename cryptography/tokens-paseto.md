## PASETO (Platform Agnostic Security Token)

* Sources
    * https://paseto.io/
    * https://dev.to/techschoolguru/why-paseto-is-better-than-jwt-for-token-based-authentication-1b0c
    * https://developer.okta.com/blog/2019/10/17/a-thorough-introduction-to-paseto
* Attempts to be a "secure by default" JWT
* Uses strong algorithms by default
* Has two flavours, each with two versions:
    1. NIST flavour
        * V1: NIST original
        * V3: NIST Modern
    1. Sodium flavour
        * V1: Sodium original
        * V3: Sodium Modern
* Seems to depend on libsodium
* A ruby lib exists but it doesn't seem very used
    * https://github.com/mguymon/paseto.rb
* Conclusion: might be usable in production today


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
