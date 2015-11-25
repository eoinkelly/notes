# HTTP Headers

http://www.mobify.com/blog/beginners-guide-to-http-cache-headers/
http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html

## Cache-control

    Cache-Control: max-age=0, private, must-revalidate

* a `,` separated list of "cache response directives"

Cache response directives

* private|public
    * asks intermediary caches not to cache the response
* no-cache
    * asks caches to re-validate the resource evyer time using the Etag header
* no-cache: field1
    * ok to cache the resource if the field1 field is stripped out
    * not all user agents support it
* no-store
    * asks cache not to store any part of the request OR the response
* max-age
    * overrides the `Expires` header
    * a number of seconds that that response can be cached for
* s-max-age
    * a header designed to talk to CDNs and other intermediary caches
    * well behaved CDNs will honor it
    * it overrides `max-age` response directive and `Expires` header if they exist
* must-revalidate
    * ask the cache to never serve stale content
    * the client is required to send the reqest headers back and check that the content is not stale
* no-transform
    * ask proxies not to convert image, video formats etc.
* proxy-revalidate
    * could have been called `s-must-revalidate`
    * aimed at proxies and CDNs rather than clients

## Expires

* the traditional way to set expiry
* is overridden by the `Cache-control` header
* good to set this too for compatibility
* date format is fixed
* Rails does not seem to set this

    Thu, 01 Dec 1983 20:00:00 GMT

## Etag

    Etag: W/"0c776997933eb60833b37beaf43814c8"

## Date

    Date: Wed, 25 Nov 2015 02:44:57 GMT
