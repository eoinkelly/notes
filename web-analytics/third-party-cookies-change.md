# Third party cookie change

- [Third party cookie change](#third-party-cookie-change)
  - [Terminology](#terminology)
  - [What are the browser rules around cookies](#what-are-the-browser-rules-around-cookies)
    - [Rails session cookie and SameSite](#rails-session-cookie-and-samesite)
    - [Cookie prefixes to avoid session fixation](#cookie-prefixes-to-avoid-session-fixation)
  - [Cookies Having Independent Partitioned State (CHIPS)](#cookies-having-independent-partitioned-state-chips)
  - [Cookie JS API](#cookie-js-api)
  - [Common analytics \& tracking services](#common-analytics--tracking-services)
    - [Google Tag Manager](#google-tag-manager)
    - [Google Analytics (GA4)](#google-analytics-ga4)
    - [Google Ads](#google-ads)
    - [Meta](#meta)

## Terminology

-   First party cookie
    -   A first-party cookie is set by the publisher's web server **or any JavaScript loaded on the website**.
    -   **Cookies created in JS are first-party**
    -   Any cookie created by Google Tag Manager etc. is considered first party because it's created in JS
-   Third party cookie
    -   Third-party cookies are cookies set in responses from domains other than the one that the page loaded from.
-   Major web browsers like Firefox and Safari have restricted third-party cookies since 2013, and Google will phase them out in 2024.

## What are the browser rules around cookies

https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies

1. Browser sends request to a `server.com` server
1. Server sends response with `Set-Cookie` header to ask browser to save a cookie
1. The cookie has the following parts (some may be missing)
    - Cookie lifetime
        - `Expires` attribute has datetime value or `Max-Age` attribute has a value
        - Session cookie
            - Has no `Max-Age` or `Expires` attribute
            - Deleted when current session ends (as defined by the browser - signed in Chrome users sessions don't end when chrome is closed)
    - `Secure;`
        - this flag tells browser to only send this cookie if connection is HTTPS (or it's `localhost`)
        - is a required attribute from some other attributes to be used
    - `HttpOnly;`
        - inaccessible to JS `document.cookie` API - only sent to server.
        - Can JS overwrite it? I think so
    - Cookie scope (Domain and Path together form the scope)
        - `Domain`
            - the domain that this cookie should be sent to
            - all subdomains are automatically included
        - `Path`
            - A path which must be in the URL in order for the `Cookie` header to be sent.
            - `/` is treated as a directory separator
            - subdirectories match as well e.g. `Path=/docs` matches `/docs/`, `/docs/web`
            - If not set, a default value is set
                - default value is `/` if any of
                    - path is empty
                    - contains no more than one `/` character
                    - does not start with `/`
    - `SameSite=Lax|Strict|None`
        - control whether this cookie is sent with cross-site requests
        - Lax
            - Default if `SameSite` not specified
            - Same as strict but will send cookie if the user navigates to the cookie's origin site from a different site
                - I think this means it'll send it on the first request???
            - Lax allows the cookie to be sent on some cross-site requests, whereas Strict never allows the cookie to be sent on a cross-site request.
            - The situations in which Lax cookies can be sent cross-site must satisfy both of the following:
                1. The request must be a top-level navigation. You can think of this as equivalent to when the URL shown in the URL bar changes, e.g. a user clicking on a link to go to another site.
                2. The request method must be safe (e.g. GET or HEAD, but not POST).
        - Strict
            - Only send the cookie with requests to the cookie's origin site (ignores `Domain`)
        - None
            - Send cookie for both originating and cross-site requests but only if `Secure` is set
1. Browser agrees to save the cookie
1. Any future requests to server.com will include the cookie

### Rails session cookie and SameSite

Q: any reason not to set rails session cookie to be Strict?
Reasons to not use `SameSite=Strict`:

-   ?? oauth, does this actually require sending (for example) google cookies to google
- ?? submitting email sign up forms or other forms which go offsite

### Cookie prefixes to avoid session fixation

A vulnerable app on foo.example.com com can set a cookie with domain `example.com` which could be used to get to other apps under `example.com` (or `example.com` itself). This is called a session fixation attack.

To prevent this you acn use prefixes on the cookie name to:

-   `__Host-` (Domain lock a cookie
    -   If a cookie name has this prefix then it is accepted by the browser only if `Secure` is set and does not have a `Domain` set and `Path=/`.
    -   confines a subdomain cookie to just that subdomain
-   `__Secure-`
    -   If cookie name has this prefix it's accepted by the browser oly if `Secure` is set and it was set by a secure origin.
    -   weaker than the `__Host-` prefix

> If the cookie domain and scheme match the current page, the cookie is considered to be from the same site as the page, and is referred to as a first-party cookie.
>
> If the domain and scheme are different, the cookie is not considered to be from the same site, and is referred to as a third-party cookie.

```
=======================
> Cookies with the SameSite=None; Secure and not Partitioned attributes that operate in cross-site contexts are third-party cookies. In future Chrome versions, setting third-party cookies will be blocked.
> https://developers.google.com/privacy-sandbox/3pcd#report-issues
```

## Cookies Having Independent Partitioned State (CHIPS)

-   Browser support: Chrome,Edge=yes, Firefox,Safari=No as of 2024-02-13
-   https://developer.mozilla.org/en-US/docs/Web/Privacy/Partitioned_cookies
-   can opt-in your cookies to partitioned storage, with a separate cookie jar per top-level site.
-   Adds a new `Partitioned;` attribute - `Secure;` must also be set.
-   Normally cookies are (conceptually) stored in a key value store with the key being the `host-key`
-   The `host-key` seems to be the full the `scheme://domain` part of the URL which loaded the page.
    -   Firefox docs call this the "origin"
-   Partitioned cookies are stored in a KV store with the key being the tuple `(host-key, partition-key)
    -   This is the "cookie origin" but that's not an official term

> This enables valid non-tracking uses of third-party cookies to continue
> working in browsers that do not allow cookies to be used for third-party
> tracking.

> The partition key is based on the scheme and eTLD+1 of the top-level URL the
> browser was visiting when the request was made to the URL endpoint that set the
> cookie.

```sh
# foo.com loads JS from analytics.com
(host-key, partition-key) == ("https://foo.com", "analytics.com")
# this cookie only sent when a request to analytics.com originates from a page loaded from foo.com

# bar.com loads JS from analytics.com
(host-key, partition-key) == ("https://bar.com", "analytics.com")
# this cookie only sent when a request to analytics.com originates from a page loaded from bar.com
```

    Q: How do these cookies present in chrome dev tools?

## Cookie JS API

```js
document.cookie; // return all accessible cookies as a single string
document.cookie = "string representing cookie"; // replace all accessible cookies with given string
```

## Common analytics & tracking services

### Google Tag Manager

-   GTM itself does not use cookies except if you use _preview and debug_ mode
-   The JS it downloads and runs can set cookies
    -   => All cookies set via GTM code are set in JS

### Google Analytics (GA4)

-   Uses only first party cookies

| cookie name          | expiry  | purpose               |
| -------------------- | ------- | --------------------- |
| \_ga                 | 2 years | identify the user     |
| \_ga\_<container_id> | 2 years | persist session state |

### Google Ads

Unclear.

### Meta

https://en-gb.facebook.com/business/help/471978536642445?id=1205376682832142 implies that Meta pixel will work fine with just first party
