# Rack

- Rack aims to provide a minimal API for connecting web servers and web
  frameworks
- Avoids having web frameworks have different handlers for different servers
- Rack is similar to Python WSGi

- Web servers that can speak rack
    - WEBRick
    - Thin
    - Passenger
    - Puma
    - Unicorn
- Frameworks that can speak rack
    - Rails
    - Sinatra
    - Mack
    - Espresso

Rack is both a spec _and_ a middleware gem that implemnets the spec.

## Rack the gem

- divides up incoming HTTP requests into different pipelined stages
- handles the stages in pieces until it sends back a response from the app
- has a
    - Handler (communicates with web server)
    - Adapter (communicates with app)

- middleware is in the _middle_ of the server and the application

## Rack Middlewares

- Use `rake about` to see a list of the middleware in use on rails

```
About your application's environment
Ruby version              2.1.1-p76 (x86_64-darwin13.0)
RubyGems version          2.2.2
Rack version              1.5
Rails version             4.1.1
JavaScript Runtime        Node.js (V8)
Active Record version     4.1.1
Action Pack version       4.1.1
Action View version       4.1.1
Action Mailer version     4.1.1
Active Support version    4.1.1
Middleware                Rack::Sendfile, ActionDispatch::Static, Rack::Lock, #<ActiveSupport::Cache::Strategy::LocalCache::Middleware:0x007ffc04a1a010>, Rack::Runtime, Rack::MethodOverride, ActionDispatch::RequestId, Rails::Rack::Logger, ActionDispatch::ShowExceptions, ActionDispatch::DebugExceptions, Raygun::Middleware::RackExceptionInterceptor, Raygun::Middleware::RailsInsertAffectedUser, ActionDispatch::RemoteIp, ActionDispatch::Reloader, ActionDispatch::Callbacks, ActiveRecord::Migration::CheckPending, ActiveRecord::ConnectionAdapters::ConnectionManagement, ActiveRecord::QueryCache, ActionDispatch::Cookies, ActionDispatch::Session::CookieStore, ActionDispatch::Flash, ActionDispatch::ParamsParser, Rack::Head, Rack::ConditionalGet, Rack::ETag, Warden::Manager, RequestStore::Middleware, MetaRequest::Middlewares::MetaRequestHandler, MetaRequest::Middlewares::Headers, MetaRequest::Middlewares::AppRequestHandler
Application root          /Users/eoinkelly/Code/app
Environment               development
Database adapter          postgresql
Database schema version   20140523033124
```

These are the 30 middlewares from a Rails 4.1 app:

1. Rack::Sendfile
2. ActionDispatch::Static
3. Rack::Lock
4. #<ActiveSupport::Cache::Strategy::LocalCache::Middleware:0x007ffc04a1a010>
5. Rack::Runtime
6. Rack::MethodOverride
7. ActionDispatch::RequestId
8. Rails::Rack::Logger
9. ActionDispatch::ShowExceptions
10. ActionDispatch::DebugExceptions
11. Raygun::Middleware::RackExceptionInterceptor
12. Raygun::Middleware::RailsInsertAffectedUser
13. ActionDispatch::RemoteIp
14. ActionDispatch::Reloader
15. ActionDispatch::Callbacks
16. ActiveRecord::Migration::CheckPending
17. ActiveRecord::ConnectionAdapters::ConnectionManagement
18. ActiveRecord::QueryCache
19. ActionDispatch::Cookies
20. ActionDispatch::Session::CookieStore
21. ActionDispatch::Flash
22. ActionDispatch::ParamsParser
23. Rack::Head
24. Rack::ConditionalGet
25. Rack::ETag
26. Warden::Manager
27. RequestStore::Middleware
28. MetaRequest::Middlewares::MetaRequestHandler
29. MetaRequest::Middlewares::Headers
30. MetaRequest::Middlewares::AppRequestHandler
