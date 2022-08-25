# Railsconf 2022

## Tenderlove keynote

Serious stuff starts at https://youtu.be/5QgQicKHmeU?t=1765

* good overview of the history of HTTP and how that impacts rack
* Rails 7 wants you to use HTTP/2 for your apps because it encourages importmap which depends on H2
    * Another way: Import maps need H2 between browsers and your app (somehow)
* Rack base on PIP 333: https://peps.python.org/pep-3333/
* Rack calls it's Hash param `env` because it is spiritually derived from CGI which passed values as actual unix environment variables
* HTTP `Keep-ALive` header tells the ??? to keep the TCP connection alive to make future HTTP requests
* You can send HTTP 103 early hints resposne from your app
  * it is designed for the h2 proxy which can consuem this and convert it into H2 push promises to the browser
  * browsers do not support HTTP 103 responses directly
* How rack supports websockets
    * Your rack app gets an `env` as normal
    * The env has a `rack.hijack` key which returns a lambada you could call to get access to the underlying TCP socket e.g. `io = env["rack.hijack"].call`
    * Example
        ```ruby
        my_rack_app = lambda do |env|
            io = env["rack.hijack"].call
            io.write("hi")
            io.close # close the socket

            # we still have to return a rack-ish response but it is never used - this is pretty odd
            [200, {}, []]
        end
        ```
* how rack supports early hints
    * Your rack app gets an `env` as normal
    * The env has a `rack.early_hints` key which returns a lambda you could call (passing it an array of file paths) to return the HTTP 103 response  e.g. `env["rack.early_hints"].call`
    * Example
        ```ruby
        my_rack_app = lambda do |env|
            # this generates the HTTP 103 response
            env["rack.early_hints"].call([
                { link: "/style.css", as: "style" },
                { link: "/script.css" }
            ])

            # this generates your actual page response
            [200, { ... }, ["hello there"]]
        end
        ```
* If browsers are talking to your H2 proxy then your H2 proxy must terminate TLS because browsers won't H2 without it
* HTTP/2 (which I'm abbreviating as H2) proxies
    * h2o
    * ? nginx
    * ? others

Misc stuff

    Q: Are there differences in how smart these proxies can be in talking H2 downstream to browsers and H1.1 upstream to our app server?
    IDEA: write a minimal app directly on Puma without using rack


## Joel Hawksley - Breaking up with the bundle

https://www.youtube.com/watch?v=21QG19Zy_g0&list=PLbHJudTY1K0f1WgIbKCc0_M-XMraWwCmk&index=3

When is the right time to introduce a design system in the lifecycle of an app?
    is it a certain num of CSS lines?
    is it when making interface changes gets time consuming and brittle? is that too late?
    If you wait until you have 40k lines of existing CSS then moving existing stuff to the design system might never happen

they wanted to be able to make changes to CSS safely without regressions
Can ViewComponents be used with scoped CSS to make CSS easier to change
    scoped CSS seems tangential to the design system

Github bundles all their CSS into one bundle and their bundle is huge and almost every changes it in some way so it breaks cache multiple times per day
=> they wanted to make separate bundles because some partials were used across many parts of the site so it was very hard to split the CSS
=> CSS in JS is a huge win in this kind of situation because it automatically keeps your CSS in the one component
They use ViewComponents and create an SCSS file for the component
    Then compile that SCSS file into it's own bundle and load it in the body of hte page with a link tag
        Browser will cache this load and not load it again if the component appears more than once on the page
            the example tg has an `integrity` attribute which I'm not sure if it's related?
        Consequence: this leans **heavily** into serving the app via H2 because you may have many many small files to load (Github is h2)


Style snapshot diffing
Diff computed styles on a node with Chrome devtools protocol
https://github.com/rubycdp/ferrum gem to help
Github has built a VCR for styles


Writing custom CSS at Github scale is a clear anti-pattern

## RailsConf 2022 - Opening Keynote: The Journey to Zeitwerk by Xavier Noria

https://www.youtube.com/watch?v=DzyGdOd_6-Y&list=PLbHJudTY1K0f1WgIbKCc0_M-XMraWwCmk&index=2

* Un-scoped constant lookup e.g. User:
    * constant lookup starts by going up through the nesting and then goes up the ancestral chain all the way up to Object then falls back to `const_missing`
* scoped constant lookup e.g. Admin::User
    * goes up the ancestral chain but skips Object (because you if not you would find Admin::String etc.) then falls back to `const_missing`
* Zeitwork can work fine outside of Rails and is being used by some gems now


## RailsConf 2022 - React-ing to Hotwire by David Hill

https://www.youtube.com/watch?v=6uj5o7U-3Y4&list=PLbHJudTY1K0f1WgIbKCc0_M-XMraWwCmk&index=4

