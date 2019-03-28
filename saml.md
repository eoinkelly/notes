# SAML 2.0

## Sources

*   https://wiki.oasis-open.org/security/FrontPage
*   https://wiki.oasis-open.org/security/Saml2TechOverview (Technical overview)
*   https://en.wikipedia.org/wiki/SAML_2.0

## Basics

* Security Assertion Markup Language
* Ratified in 2005
* SAML 2.0 is an XML-based protocol
* used for
  1. authentication
  1. authorisation
* enables
  *   web based, cross domain single sign on
*   SAML is many things. It is:
    1. An XML based markup language for security assertions
    2. A set of XML based protocol messages
    3. A set of protocol "message bindings"
        * A SAML binding is a mapping of a SAML protocol message onto standard
          messaging formats and/or communications protocols.
        * For example, the SAML SOAP binding specifies how a SAML message is
          encapsulated in a SOAP envelope, which itself is bound to an HTTP
          message.
        * SAML 2.0 defines multiple bindings:
            * SOAP binding (based on SAML 1.1)
            * Reverse SOAP binding (PAOS)
            * HTTP Redirect (GET) binding
            * HTTP POST binding
            * HTTP Artifact binding
            * SAML URI binding
    4. A set of profiles which combine the markup-language, Protocol messages and message bindings (1, 2, 3 above) together for a particular use-case
        * There are 9 profiles in SAML 2.0 (5 of which are SSO Profiles)
        * The most important profile is _Web Browser SSO Profile_
* Is made up of 4 standards
    1. Core (assertions & protocols)
        * SAML Assertion format is also used in the _WS Federation_ standard
    1. Bindings
    1. Metadata
    1. Profiles
* Because it is a standard, it can be used by many different "federation partners"
* You can get your SAML 2.0. implementation certified by Kantara after interoperability testing
* SAML 2.0 is built on SAML 1.1, Shibboleth, Liberty ID-FF
* uses security tokens containing assertions to pass information about a
  "principal" (usually an end user) between a SAML authority (an identity
  provider) and a SAML consumer service provider
* Actors in a SAML:
  1. The principal (typically a human user, via a web-browser/user-agent)
      * sometimes also called _subject_
  1. The _identity provider (IdP)_
      * _asserts_ the identity of the user
  1. The _service provider (SP)_
      * consumes that assertion and passes identity information to the application
* In the "Lightweight Web Browser SSO Flow" all exchanges between actors are "front-channel" i.e. the IdP and SP never talk directly - they only talk to the Principal (or their user-agent/browser)
  *   there are no back-channel exchanges or direct communications between the service provider and the identity provider
* There are other profiles where back-channel exchanges happen
  *   these can be more secure because the principal can be given a reference to values held by the SP and IdP without actually giving them the values

The flow

1. Principal (subject) requests a resource from the SP
1. SP accepts the request and sends the principal to the IdP, passing on some state of its own
1. IdP gets the request and does somthing to verify that the principal is who they say the are (this bit is unspecified in SAML)
1. IdP redirects the principal back to a special endpoint on the SP which can consume the assertion from the IdP
1. The SP verifies the assertion from the IdP (the principal is now authenticated)
1. The SP decides whether to grant the now authenticated princpal access to the resource (i.e. authorization happens)
1. The principal either gets the resource or an error
