# Javascript frameworks/word salad

## Full stack

1. NestJS
    - https://nestjs.com/
    - https://github.com/nestjs/nest
    - Stars: 53.2k
    - Uses annotations(which flavour), looks somewhat railsy
    - Nest provides tight integration with TypeORM and Sequelize out-of-the-box
      but you can use any ORM
    - dominates popularity
      https://nodejstoolbox.com/categories/full-stack-frameworks
1. SailsJS
    - https://sailsjs.com/
    - https://github.com/balderdashy/sails
    - Stars: 22.4k
    - uses Express
1. FeathersJS
    - https://feathersjs.com/
    - https://github.com/feathersjs/feathers
    - Stars: 14.3k
    - self describes as
        - being for APIs and real-time applications
        - being similar to Sails
        - uses Express
1. AdonisJS
    - https://adonisjs.com/
    - https://github.com/adonisjs/core
    - Stars: 13.4k
    - Looks very Rails like
1. RedwoodJS
    - https://redwoodjs.com/
    - seems rails like

## Sinatra alike/api focus

1. Express
    - https://expressjs.com/
    - https://github.com/expressjs/expressjs.com
    - Stars: 4.7k
    - More sinatra than rails
2. Hapi
    - https://hapi.dev/
    - self-describes as minimal
    - https://github.com/hapijs/hapi
    - Stars: 14.1k
3. Loopback
    - https://loopback.io/
    - https://github.com/loopbackio/loopback-next
    - Stars: 4.4k
    - self describes as being for building APIs and microservices.
4. Koa
    - https://koajs.com/
    - https://github.com/koajs/koa
    - same team who made express
    - seems to be aiming to be a better Express
    - Stars 33.5k
5. Restify
    - http://restify.com/
    - https://github.com/restify/node-restify
    - Stars: 10.6k
    - For APIs not Rails

## Front-end framework with a nodejs SSR/"JAMStack framework"/static-site++ frameworks

Characteristics:

- Define you component once and it can
    - run on server
    - run on client
    - be built into a static page
- Creates enough server to serve your front-end
    - Doesn't really try to address anything beyond that - some have ability to
      create API server endpoints but they are often paired with a separate API
      server
- Have very strong opinions on how you render pages - use their component model,
  no "just render HTML" templates
- could be thought of as "static site++" - spiritually a static site but with a
  necessary server component.
    - doesn't address the needs of a rich backend server
- Spiritually your frontend now runs both in browser and server. Even though it
  runs on server, that doesn't mean it cares about the same things as a
  traditional backend would

Options are:

- Remix
    - https://remix.run/
    - https://github.com/remix-run/remix
    - fairly new, uses React for everything server and client side
    - Stars: 21.4k
    - a full-stack web framework
    - uses TSX which presumably is JSX but typescript
- Sveltekit
    - https://kit.svelte.dev/
    - https://github.com/sveltejs/kit
    - 12.3k
    - is to Svelte what Next.js is to React
    - A framework built on Svelte and Vite
    - does both server and client rendering
- Next JS
    - By Vercel
    - A framework for React
    - "rust based tooling "
    - v13 is latest
    - has a CLI
- Fresh (Deno)
    - https://fresh.deno.dev/
    - Deno based web framework
    - SSR, uses islands approach like Astro
    - seems to be similar in approach to Astro
- Astro
    - https://docs.astro.build/en/concepts/why-astro/
    - Aimed primarily for content sites
    - is a static site builder out of the box but you can add dapters to do SSR
        - https://docs.astro.build/en/guides/integrations-guide/node/
        - astro can be a standalone server or a middleware for things like
          Express
    - Does have SSR
    - Uses file based routing (precompiles components into static HTML)
    - Firmly a MPA approach - out of the box there is no client side rendering
        - it seems like it's mostly precompiled into static HTML and CSS
        - not sure how you add client side JS yet
    - Netlify is their offical hosting partner
    - > Astro is an MPA framework. Traditional MPA frameworks also include Ruby
      > on Rails, Python Django, PHP Laravel, WordPress, Joomla, Drupal and
      > static site builders like Eleventy or Hugo
    - Storyblok is their official CMS partner
    - can pull data from many CMSes and sources
    - serves 0 js to the client by default
        - it builds static pages by default I think. you have to explicitly
          enable SSR via an adapter for your platform
    - you create `.astro` file components which are HTML and JS separated by
      `---` line
        - has sugar for referencing JS vars from HTML using JSX alike syntax
    - uses vite
- Nuxt JS
    - A framework built on Vue
    - seems to be a "NextJS for Vue" (or perhaps this came first)
- Gatsby
    - A react based framework
    - owned by Netlify
    - might be kinda dead as of end 2023
    - uses a graphql for the data layer which can be backed by many kinds of CMS
      and data sources
    - has a "Gatsby cloud" offering
- Qwik
    - Uses vite
    - Created by the AngularJS guy
    - components use JSX but are not React
    - builds into a static site I think
    - markets as being able to do SPA or MPA
    - can run react components natively
    - Seems heavily supported/sponsored by https://www.builder.io/

## Static site generators

- Eleventy
    - A static site generator (another Jekyll in JS thing)
- Hugo
    - A static site generator written in go-lang

## Dev tools

- esbuild
    - an asset bundler
    - written in go-lang, aims to be very fast
- Vite
    - a dev server which does HMR
    - a bundler which uses rollup
    - aims to be fast

## Component libraries

- React
    - the big dog
- Svelte
    - competes with React
    - doesn't do DOM diffing
- SolidJS
    - https://www.solidjs.com/
    - competes with React
    - uses reactivity
        - component lifetime is while it is on page vs react's "alive for a
          single render" approach
        - uses signals for reactivity
- Marko
    - https://markojs.com/
    - extends HTML to have JS and CSS
    - seems to compete with React and/or JSX
