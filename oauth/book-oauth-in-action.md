# OAuth2 in Action

- OAuth2 is all about delegating access - a user delegates **part** of their
  authority to a client application
- OAuth2 calls itself an _authorization protocol_ but is actually a _delegation
  protocol_ - the thing being delegated is a part of the user's authorization!
    - OAuth2 by itself does not carry or convey any authorizations
- Oauth is a delegation protocol which lets a client application access a
  protected resource **without impresonating the user who owns it**

> Only the protected resource needs to know the authorization, and as long as it
> is able to find out from the token and its presentation context (either by
> looking at the token directly or by using a service of some type to get this
> information), it can serve the API as required.

- Authorizations are opaque to most of the players in the OAuth2 system (except
  for the resource server)
    - it can decide on the authorization by
        1. direct inspection of the token (if the token type supports it)
        2. using a service to validate the token
- OAuth2 moves complexity away from the clients and onto the server
- OAuth2
    - is NOT an authentication protocol
        - it does have a few uses of authentication (e.g. client app
          authenticates itself to auth server) embedded but that does not make
          it an authentication protocol
    - does NOT define a token format
    - does not define how authorizaiton processing happens
        - OAuth will tell you that authorization happened
        - it provides the tools but it is up to you to define the meaning of the
          scopes and define what actions a particular token authorizes
    - is NOT a single protocol
        - there are four main grant types
    - does NOT define any cryptographic methods
        - this deliberate ommision has lead to the development of JSON object
          signing and encryption (JOSE) suite of specifications

### Aside: LDAP authentication

- user enters password into ldap-client and the client replays it to the LDAP
  server to verify that it is correct
- LDAP authentication is basically a benign MITM attack

### Three layer security and TOFU

- OAuth systems follow the principle of **Trust On First Use** (TOFU)
    - The system will remember the user's choice the first time a particular
      "authorization context" was met and will use that choice the next time the
      same authorization context occurs

Security architects often use a three layer system of

1. Whitelist
    - contains:
        - internal parties
        - trust frameworks
        - known business partners
        - customer organisations
    - Managed by:
        - centralized control
        - traditional policy management
2. Greylist
    - contains: unknown entities
    - use trust on first use (TOFU)
    - have rules for when to move them to whitelist or blacklist
    - do extensive logging and auditing
3. Blacklist
    - contains: known bad parties, attack sites
    - Managed by:
        - centralized control
        - traditional policy management

For example:

If you have customer data in a CRM you could take the list of software
applications which can access the CRM and divide it into the three groupings
abovie

1. Whitelist
    - internal applications which are trusted by default
2. Greylist
    - unknown applications with unknown intentions
    - they are trusted after the first use if the user in question trusts them
    - they are heavily logged and audited
    - they can be promoted to whitelist or demoted to blacklist based on audits
3. Blacklist
    - know malicious apps

Chapter 12 promises to deal with this more

UP TO END CHAP 1
