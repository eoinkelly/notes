# Rack

* Currently on version 2.x

https://github.com/rack/rack


## Rails usage of rack

Rails 6 uses the following middlewares (from rack itself) as part of its middleware stack:

1. use Rack::Sendfile
2. use Rack::Runtime
3. use Rack::MethodOverride
4. use Rack::Head
5. use Rack::ConditionalGet
6. use Rack::ETag
7. use Rack::TempfileReaper

Rails does not seem to use any middlewares from rack-contrib

## Middlewares

* Rack + Rack-Contrib = 52 middlewares you can use to create web apps

Rack ships with 23 middlewares

1. Rack::Chunked, for streaming responses using chunked encoding.
2. Rack::CommonLogger, for creating Apache-style logfiles.
3. Rack::ConditionalGet, for returning not modified responses when the response has not changed.
4. Rack::Config, for modifying the environment before processing the request.
5. Rack::ContentLength, for setting Content-Length header based on body size.
6. Rack::ContentType, for setting default Content-Type header for responses.
7. Rack::Deflater, for compressing responses with gzip.
8. Rack::ETag, for setting ETag header on string bodies.
9. Rack::Events, for providing easy hooks when a request is received and when the response is sent.
10. Rack::Files, for serving static files.
11. Rack::Head, for returning an empty body for HEAD requests.
12. Rack::Lint, for checking conformance to the \Rack API.
13. Rack::Lock, for serializing requests using a mutex.
14. Rack::Logger, for setting a logger to handle logging errors.
15. Rack::MethodOverride, for modifying the request method based on a submitted parameter.
16. Rack::Recursive, for including data from other paths in the application, and for performing internal redirects.
17. Rack::Reloader, for reloading files if they have been modified.
18. Rack::Runtime, for including a response header with the time taken to process the request.
19. Rack::Sendfile, for working with web servers that can use optimized file serving for file system paths.
20. Rack::ShowException, for catching unhandled exceptions and presenting them in a nice and helpful way with clickable backtrace.
21. Rack::ShowStatus, for using nice error pages for empty client error responses.
22. Rack::Static, for more configurable serving of static files.
23. Rack::TempfileReaper, for removing temporary files creating during a request.

After a while all new middleware was directed to https://github.com/rack/rack-contrib instead of rack itself

Rack contrib (as of 2020-08-08) has 29 extra middlewares

1. `Rack::Access` - Limits access based on IP address
2. `Rack::Backstage` - Returns content of specified file if it exists, which makes it convenient for putting up maintenance pages.
1. `Rack::BounceFavicon` - Returns a3 for requests to `/favicon.ico`
4. `Rack::CSSHTTPRequest` - Adds CSSHTTPRequest support by encoding responses as CSS for cross-site AJAX-style data loading
5. `Rack::Callbacks` - Implements DSL for pure before/after filter like Middlewares.
6. `Rack::Cookies` - Adds simple cookie jar hash to env
7. `Rack::Deflect` - Helps protect against DoS attacks.
8. `Rack::Evil` - Lets the rack application return a response to the client from any place.
9. `Rack::HostMeta` - Configures `/host-meta` using a block
10. `Rack::JSONBodyParser` - Adds JSON request bodies to the Rack parameters hash.
11. `Rack::JSONP` - Adds JSON-P support by stripping out the callback param and padding the response with the appropriate callback format.
12. `Rack::LazyConditionalGet` - Caches a global `Last-Modified` date and updates it each time there is a request that is not `GET` or `HEAD`.
13. `Rack::LighttpdScriptNameFix` - Fixes how lighttpd sets the `SCRIPT_NAME` and `PATH_INFO` variables in certain configurations.
14. `Rack::Locale` - Detects the client locale using the Accept-Language request header and sets a `rack.locale` variable in the environment.
15. `Rack::MailExceptions` - Rescues exceptions raised from the app and sends a useful email with the exception, stacktrace, and contents of the environment.
16. `Rack::NestedParams` - parses form params with subscripts (e.g., * "`post[title]=Hello`") into a nested/recursive Hash structure (based on Rails' implementation).
1. `Rack::NotFound` - A default17 application.
18. `Rack::PostBodyContentTypeParser` - [Deprecated]: Adds support for JSON request bodies. The Rack parameter hash is populated by deserializing the JSON data provided in the request body when the Content-Type is application/json
19. `Rack::Printout` - Prints the environment and the response per request
20. `Rack::ProcTitle` - Displays request information in process title (`$0`) for monitoring/inspection with ps(1).
21. `Rack::Profiler` - Uses ruby-prof to measure request time.
22. `Rack::RelativeRedirect` - Transforms relative paths in redirects to absolute URLs.
23. `Rack::ResponseCache` - Caches responses to requests without query strings to Disk or a user provided Ruby object. Similar to Rails' page caching.
24. `Rack::ResponseHeaders` - Manipulates response headers object at runtime
25. `Rack::Signals` - Installs signal handlers that are safely processed after a request
26. `Rack::SimpleEndpoint` - Creates simple endpoints with routing rules, similar to Sinatra actions
27. `Rack::StaticCache` - Modifies the response headers to facilitiate client and proxy caching for static files that minimizes http requests and improves overall load times for second time visitors.
28. `Rack::TimeZone` - Detects the client's timezone using JavaScript and sets a variable in Rack's environment with the offset from UTC.
29. `Rack::TryStatic` - Tries to match request to a static file

## Handlers

Rack ships with _handlers_ for ? web servers. All other Ruby servers ship with Rack support

## Helper classes

Rack ships with 9 helper classes to help you do development with it

1. Rack::Request, which also provides query string parsing and multipart handling.
2. Rack::Response, for convenient generation of HTTP replies and cookie handling.
3. Rack::MockRequest and Rack::MockResponse for efficient and quick testing of \Rack application without real HTTP round-trips.
4. Rack::Cascade, for trying additional \Rack applications if an application returns a not found or method not supported response.
5. Rack::Directory, for serving files under a given directory, with directory indexes.
6. Rack::MediaType, for parsing Content-Type headers.
7. Rack::Mime, for determining Content-Type based on file extension.
8. Rack::RewindableInput, for making any IO object rewindable, using a temporary file buffer.
9. Rack::URLMap, to route to multiple applications inside the same process.

## "Rack up" DSL

* Rack has a Ruby DSL for creating a server
* It expects to be in a `.ru` ("dot rack up") file

```
# you can create an app instance from a config.ru file
MY_APP = Rack::Builder.parse_file("config.ru").first
```
## Rack builder

???


## Testing

Rack itself ships with `Rack::MockRequest` and `Rack::MockResponse` to help with testing

`rack-test` is a layer on top of these built-in helpers.

https://github.com/rack/rack-test

Rack::Test’s helper methods expect that there’s a method app defined, so they can call it.

```
# mixin this module to get access to the methods on Rack::Test::Session
include Rack::Test::Methods
```

Rack::Test is usually included like this:

```ruby
# spec_helper.rb
require "rack/test"

RSpec.configure do |config|
  config.include Rack::Test::Methods
end
```
