# SAML 2.0

## Background on XML Namespaces

- a way of defining a vocabularly for a set of tag names in an XML document
- each namespace has a "name" which functions as a URI
    - namespace names are often in the form of a HTTP URL
        - from the XML parsers point of view they are just an opaque string
        - using a HTTP url helps avoid namespaces clashing assuming everybody
          uses a URL for a website they control
            - e.g. if I use `https://me.mysite.com/xhtml` for my namespace I
              won't clash with other `xhtml` namespaces
        - there is **no requirement that namespace names actually resolve to
          anything useful**

Namespace prefixes

- You can create a shorthand way to reference a namespace in your document
  called a prefix
- any element in the document whose name begins with `my_prefix:` will be in the
  `my_prefix` namespace which has (earlier in the document) been mapped to its
  full namespace name via a namespace declaration e.g.
    ```
    xmlns:my_prefix="http://www.me.mysite.com/mystuff"
    ```
- The namespace declartion must be at or above the elements that use it in the
  document tree
- you can also declare a default namespace in a document via

    ```
    xmlns="http://www.w3.org/1999/xhtml"
    ```

- default namepsace
    - If a document has no default namespace declaration then the namespace is
      considered to be empty i.e. any element without an explicit namespace
      prefix is considered to have no namespace
    - A convention seems to be to declare the namespace prefix on the highest
      element in the doc that uses that namespace i.e. the namespace prefix
      declaration appears on the first element that uses it

Example:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!--
Note that we declare multiple namespaces prefixes in the root element of the document. These prefixes can be used on the root elemetn itself (and they are) or its children
-->
<samlp:LogoutRequest
    xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"
    xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
    ID="_f36d62641b17513f2501abcc0d8eee174b8fbb13bd"
    IssueInstant="2020-04-28T23:09:14Z"
    Version="2.0"
    Destination="https://dpc-saml-demo.herokuapp.com/users/saml/idp_sign_out">
  <saml:Issuer xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion">http://demo.com/?q=admin/config/people/miniorange_saml_idp/</saml:Issuer>
  <saml:NameID xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"></saml:NameID>
</samlp:LogoutRequest>
```

There are three namespaces which appear commonly in SAML messages:

```

xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol"
    SAML protocol vocabularly

xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion"
    SAML assertion vocabularly

xmlns:ds="http://www.w3.org/2000/09/xmldsig#"
    XML Signatures vocabularly
```

## Sources

- https://wiki.oasis-open.org/security/FrontPage
- https://wiki.oasis-open.org/security/Saml2TechOverview (Technical overview)
- https://en.wikipedia.org/wiki/SAML_2.0

## Basics

- Security Assertion Markup Language
- Ratified in 2005
- SAML 2.0 is an XML-based protocol
- used for
    1. authentication
    1. authorisation
- enables
    - web based, cross domain single sign on
- SAML is many things. It is:
    1. An XML based markup language for security assertions
    2. A set of XML based protocol messages
    3. A set of protocol "message bindings"
        - A SAML binding is a mapping of a SAML protocol message onto standard
          messaging formats and/or communications protocols.
        - For example, the SAML SOAP binding specifies how a SAML message is
          encapsulated in a SOAP envelope, which itself is bound to an HTTP
          message.
        - SAML 2.0 defines multiple bindings:
            - SOAP binding (based on SAML 1.1)
            - Reverse SOAP binding (PAOS)
            - HTTP Redirect (GET) binding
            - HTTP POST binding
            - HTTP Artifact binding
            - SAML URI binding
        - In practice, most SAML happens over a combination of HTTP POST, HTTP
          Redirect (all over HTTPS)
    4. A set of profiles which combine the markup-language, Protocol messages
       and message bindings (1, 2, 3 above) together for a particular use-case
        - There are 9 profiles in SAML 2.0 (5 of which are SSO Profiles)
        - The most important profile is _Web Browser SSO Profile_
- Is made up of 4 standards
    1. Core (assertions & protocols)
        - SAML Assertion format is also used in the _WS Federation_ standard
    1. Bindings
    1. Metadata
    1. Profiles
- Because it is a standard, it can be used by many different "federation
  partners"
- You can get your SAML 2.0. implementation certified by Kantara after
  interoperability testing
- SAML 2.0 is built on SAML 1.1, Shibboleth, Liberty ID-FF
- uses security tokens containing assertions to pass information about a
  "principal" (usually an end user) between a SAML authority (an identity
  provider or IdP) and a SAML consumer service provider (SP)
- Actors in a SAML:
    1. The principal (typically a human user, via a web-browser/user-agent)
        - sometimes also called _subject_
    1. The _identity provider (IdP)_
        - _asserts_ the identity of the user
    1. The _service provider (SP)_
        - consumes that assertion and passes identity information to the
          application
- In the "Lightweight Web Browser SSO Flow" all exchanges between actors are
  "front-channel" i.e. the IdP and SP never talk directly - they only talk to
  the Principal (or their user-agent/browser)
    - there are no back-channel exchanges or direct communications between the
      service provider and the identity provider
- There are other profiles where back-channel exchanges happen
    - these can be more secure because the principal can be given a reference to
      values held by the SP and IdP without actually giving them the values

The flow

1. Principal (subject) requests a resource from the SP
1. SP accepts the request and sends the principal to the IdP, passing on some
   state of its own
1. IdP gets the request and does somthing to verify that the principal is who
   they say the are (this bit is unspecified in SAML)
1. IdP redirects the principal back to a special endpoint on the SP which can
   consume the assertion from the IdP
1. The SP verifies the assertion from the IdP (the principal is now
   authenticated)
1. The SP decides whether to grant the now authenticated princpal access to the
   resource (i.e. authorization happens)
1. The principal either gets the resource or an error

SAML provides two important features

1. Cross domain single sign-on
1. Identity federation

Q: how are these diff?

### Q: how is the trust relationship between the IDP and SP setup?

Before SAML can be used, a trust relationship must be setup between the SP and
IDP. They do this by exchanging metadata which contains

1. URL endpoints
1. Certificates which can be used to validate digitally signed messages

> To establish the ability to do cross-domain web single sign-on, the
> organizations owning the service provider (application) and identity provider
> exchange information, known as metadata. The metadata information contains
> information such as URL endpoints and certificates with which to validate
> digitally signed messages. This data enables the two parties to exchange
> messages. The metadata is used to configure and set up a trust relationship
> between the service provider and the identity provider and must be done before
> the identity provider can authenticate users for the service provider

### Cross domain single sign-on

THis is the _SP Initiated flow_

1. You attempt to login at SaaS service #1
1. You get redirected to your SAML IDP
    - This is called a _SAML Authentication Request message_
1. You do login at the IDP
    - If you already have a session cookie for the IDP then you won't be asked
      for your credentials again - the IDP will immediately generate the _SAML
      authentication response message_ - this is how the "single" in single
      sign-on works
1. The SAML response (which contains info about the authentication event and you
   as a user/subject) of your login is sent to SaaS Service #1
    - This is called a _SAML Authentication Response message_
    - it is sent to the "Assertion consumer service URL" on the SP
    - this message can be long
    - it can contain arbitrary attributes about the user/subject - the exact
      attributes vary between IDPs
1. You attempt to access SaaS Service #2
1. You are automatically logged in

Q: how does this work with HTTP POST - I get how it works with redirects and
putting the saml request and response in the URL

> The HTTP-Redirect binding can be used with SAML V2.0 requests that are not
> digitally signed, but production environments are recommended to use signed
> requests to prevent request tampering. If a request is digitally signed, it
> typically needs to be sent using the HTTP-POST binding to avoid issues with
> browser URL size limits. The response or assertion from the identity provider
> must be digitally signed. Due to the size of a signed response, the HTTP-POST
> binding is typically used for responses.

It seems to me like SAML requests must always happen over HTTP redirects

If you have to use HTTP POST and not redirects then I'm guessing the user sees
an extra screen every step of the protocol that would have just been a redirect
handled automatically by the browser You can embed some JS like

```
window.onload = function () { document.forms[0].submit(); }
```

on the page with the form to prevent the user having to click on stuff

Q: What are the ways you can misconfigure SAML?

> The link between an identity at a service provider and an identity provider
> can be set up in different ways. In practice, a user’s email address is often
> used as the identifier for a user at both the service provider and identity
> provider, but this can be problematic as a user may need to change their email
> address, and it can conflict with privacy requirements. The use of a specific
> identifier attribute can be requested dynamically in a request, or an identity
> provider can be configured to send a particular identifier to a service
> provider. It is also possible for an identity provider and service provider to
> exchange information using an opaque, internal identifier for a user, that is
> mapped on each side to the user’s profile. Use of a unique identifier for each
> federation is privacy-friendly and prevents correlation of user activity, but
> isn’t common in practice

Aside: SAML IDPs can behave as SPs and pass you on to another IDP which knows
how to log you in

https://kantarainitiative.org/trustoperations/iop-saml/

more in chap 11

HOW?

Q: what gets stored in the DB of the saas service do they store an email address
which needs updating does saml have scopes?

Before it's sent, the message is deflated (without header and checksum),
base64-encoded, and URL-encoded, in that order. Upon receipt, the process is
reversed to recover the original message.

> In practice, all the data contained in a <samlp:AuthnRequest>, such as Issuer
> which contains the SP ID, and NameIDPolicy, has been agreed between IdP and SP
> beforehand (via manual information exchange or via SAML metadata). In that
> case signing the request is not a security constraint. When the
> <samlp:AuthnRequest> contains information not known by the IdP beforehand,
> such as Assertion Consumer Service URL, signing the request is recommended for
> security purposes.

Misc stuff frm the spec

relayState should be 80 chars or less for HTTP redirect binding

if the saml request includes RelayState the the response **must** include the
same chunk of RelayState if the saml request doesn't include RelayState then the
responder can do what they want (based on being in a profile or maybe some prior
agreement)

In HTTP redirect binding

The default encoding for the saml param is
`urn:oasis:names:tc:SAML:2.0:bindings:URL-Encoding:DEFLATE` If you want to use a
different incoding you need to add a `SAMLEncoding` parameter

Steps to encode a SAML message encoded using the default DEFLATE method

    rawSamlMessage = "..."
    encodedMessage = urlencode(base64_encode(deflate(rawSamlMessage)))

SLO profile

SAML does not cover the use-case of wantting to terminate your session at one SP
but keep your sessions at the IDP and other SPs

SessionIndex are the identifiers that the SP can use to identify the session it
has with the IDP

## Types

- The basic types (string, timestamps etc.) in SAML documents are defined by the
  XML `xs` namespace

## Time

- values have type `xs:dateTime`
- MUST be expressed in UTC form with no time zone
- times should only be compared down to milliseconds
- SHOULD allow for reasonable clock skew between systems. The spec says 3-5 mins
  are reasonable defaults

## layers

- At the protocol layer, the conversation is between a _requester_ and a
  _responder_
- At the assertion layer, the conversation is between the _asserting party_ and
  the _relying party_
- An "assertion" is a package of 0-many statements about a subject made by a
  relying party

## Anatomy of an assertion

- The subjec tof the assertion is usually specified in the `<Subject>` element
  but that is optional
- SAML defines three kinds of assertion
    1. Authentication: this subject by a particular means at a particular time
    1. Attribute: this subject is associated with the given attributes
    1. Authorization decision: A request to allow the subject to access a
       specific resource has been granted or denied or is indeterminate
- SAML allows the assertion schema to be extended (i.e. new XML elements can be
  added)
