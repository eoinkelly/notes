## PASETO (Platform Agnostic Security Token)

* Sources
    * https://fly.io/blog/api-tokens-a-tedious-survey/
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

