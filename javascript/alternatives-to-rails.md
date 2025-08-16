# Context: I want to solve a problem with NodeJS that I'd normally use Rails/Phoenix for

## Sources

- https://2018.stateofjs.com/back-end-frameworks/overview/
- https://loopback.io/resources/#compare

### Rack equivalents (approxmiately)

- Expressjs
    - By far the most popular
    - Used as a basis for other frameworks
- Koa
    - https://koajs.com/
    - made by the people who made Express to be a sort of successor to express
    - requires modern NodeJS, uses async functions instead of callbacks
      extensively

### Rails equivalents (approximately)

- Hapi
    - https://hapijs.com/
    - Seems more full featured than Rack
    - can render templates via _Vision_
    - doesn't include an ORM but has plugins which integrate _Objection_ and
      _Waterline_
- Loopback
    - Part of IBM Strongloop
    - Commercially supported by IBM
    - Seems fairly complete
- NestFramework
    - https://nestjs.com/
    - Built on Express
    - Seems quite complete
- Sails
    - https://sailsjs.com/
    - Seems closest to Rails
    - Built on ExpressJS
    - ORM is Waterline

### ORMs

- TypeORM
    - https://typeorm.io/#/
    - Seems to be a fairly complete ORM (supports many DBs)
- Waterline
    - Used by sailsjs
- Objection
    - https://vincit.github.io/objection.js/

Non-options

- NextJS
    - https://nextjs.org/
    - Server rendered static site (wraps React)

# Conclusion 2019-04-28

- The phrase _backend framework_ sometimes means "server rendered front-end" and
  sometimes means "thing you use to build apps which render HTML/JSON"
- There seem to be some "Rails-esque" options but none of them seem as popular
  as Express in the JS community"
