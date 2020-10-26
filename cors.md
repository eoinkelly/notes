# CORS

> A resource makes a cross-origin HTTP request when it requests a resource from
> a different domain than the one which served itself. For example, an HTML
> page served from `http://domain-a.com` makes an `<img>` src request for
> `http://domain-b.com/image.jpg`.

* CORS was defined in its own spec but the Fetch spec overrides it

## Same origin policy

> Two pages have the same origin if the protocol, port (if one is specified), and host are the same for both pages.

Note the path part of the URL is not inspected

If HTML is loaded from `http://foo.com/index.html` then

```
https://foo.com/index.html          no match (different protocol)
http://other.com/index.html         no match (different domain)
http://foo.com:8080/index.html      no match (different port)
http://foo.com/things/blah.html     matches even though path is different
```

There are a number of "cross origin" things that the browser can do

1. cross-origin writes
    * are allowed by browser
    * examples: links, redirects, form submissions
    * this is a security problem e.g. a form on the page could be tricked into submitting to a different server
    * CSRF tokens prevent this problem
1. cross-origin embedding
    * are allowed by browser
    * examples: CSS, JS, images from different origins embedded on the page
    * embedding resources can leak information that allows scripts on the page to "read" information about those resources e.g.
        * width and height of images
        * whether the resource exists or not
1. cross-origin reads
    * are typically blocked by browser
    * use CORS to make exceptions

# Types of CORS requests

1. simple requests (do not need preflight)
    * GET|POST|HEAD with a limited subset of HTTP headers
    * makes CORS backwards compatible
2. complex requests (need preflight)
    * anything else (any other verbs or using any headers outside the allowed set)
    * these requests may send user data to the server so the browser does a "preflight" request to check that the server is happy to get it first

## Simple requests

A simple cross-site request is one that meets *all* the following conditions:

The only allowed methods are:

* GET
* HEAD
* POST

Apart from the headers set automatically by the user agent (e.g. Connection,
User-Agent, etc.), the only headers which are allowed to be manually set are:

* Accept
* Accept-Language
* Content-Language
* Content-Type

The only allowed values for the Content-Type header are:

* application/x-www-form-urlencoded
* multipart/form-data
* text/plain

Implications

* the "simple" thing allows existing pages which submit forms to other domains to continue to work
* it creates enough exceptions to not break backwards compatbililty
* XML and JSON requests are not "simple"
* simple requests are cross-origin requests that do not need CORS handshake

## Non-simple CORS requests

The headers are explained in the spec at http://www.w3.org/TR/cors/#syntax

All CORS specific headers are prefixed with `Access-Control`

1. The browser sends an `OPTIONS` request for the resource e.g. `OPTIONS
   /transactions/23 HTTP/1.1` and some HTTP headers explaining its question.
   The browser calculates these headers by inspecting the XMLHttpRequest object
   that the embedded script is using for the request
    1. `Origin: <the-origin-url>`
        * A valid CORS request _always_ contains an Origin header
        * the presence of an Origin header does not necessairily mean this is a CORS request
            * Firefox sends Origin on same-origin POST/PUSH/DELETE request
    2. `Access-Control-Request-Method:`
        * values: `GET | POST | <other http header>`
        * the HTTP method the browser wants to use
    3. `Access-Control-Request-Headers:` UPPERCASE-HTTP-HEADER, ...
        * headers that are outside those automatically set by the browser or included in the allowed list for "simple" requests
2. The server indicates what it will allow the browser to do using headers in its reponse to an OPTIONS request e.g.
    1. `Allow:` (what HTTP verbs are allowed)
    2. `Access-Control-Allow-Methods:`
        * the HTTP methods that the server will allow for this resource
    3. `Access-Control-Allow-Headers:`
        * values: `<header name>, <header name> ...`
        * indicates which headers can be used when making the main request
        * only present in pre-flight response
    4. `Access-Control-Allow-Credentials:`
        * values: `true | false`
        * by default XMLHttpRequest will not send credential info e.g. cookies or HTTP basic auth
        * you can explictly turn this on in XMLHttpRequest
        * this header lets the server declare whether it will accept credential info (e.g. cookies and) from the browser
        * if the value of this header is false the _browser_ will reject XMLHttpRequests that try to send credentials
    5. `Access-Control-Allow-Origin:`
        * values: `<origin>, <origin> | null | *`
        * the wildcard origin `*` can only be used if the client is not providing credentials
        * http://www.w6.org/TR/cors/#access-control-allow-origin-response-header
    7. `Access-Control-Max-Age:`
        * values: `<delta-seconds>`
        * the no. of seconds a response can be cached
    8. Vary: Origin
        * not strictly part of CORS spec but required to get correct caching behviour
3. The browser checks to see if the server responded correctly, if so
4. The browser sends the main request

The first three steps are called the "CORS handshake"

Note: For spec compliance and correct caching behavior, ALWAYS add the Vary:
Origin response header for CORS-enabled resources, even for non-CORS requests
and those from a disallowed origin


### How a browser decides whether this is a CORS request

```
If request URL is in "same-origin" as the URL page was loaded from (see above)
    # make a same-origin request
else
    if request uses a given subset of HTTP verbs and headers # (i.e. is simple)
        # make a simple CORS request
    else
        # make a complex CORS request
        OPTIONS /url HTTP/1.1
        Origin: ...
        Access-Control-Request-Headers: ...
        Access-Control-Request-Method: ...

        send main request
        POST /url HTTP/1.1
    end
end
```

## IE9 and older does it differently

* IE 8/9 has it's own way of doing CORS so some things e.g. angular does not support it. Ouch.

## JSONP

* An older alternative to CORS
* Not as good.

## What if you can't enable either JSONP or CORS

* You can setup a proxy on your own server that will forward requests from browsers (obviously not ideal in many respects)

## Cordova considerations

??? what is the Origin header set to in a cordova app?
??? the page is loaded from the filesystem so it might be `file://path/to/index.html`?

TODO check into this

## Sources

* http://www.w3.org/TR/cors/
* https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
* http://www.html5rocks.com/en/tutorials/cors/ in particular the server flow-chart is good
