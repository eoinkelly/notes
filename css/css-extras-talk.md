# Establishing a browser support matrix with the client

- Try to use data if you can e.g. google analytics from their existing website
- Otherwise explain the trade-offs
- Establish two sets of browsers
    1. those you support
    2. those you will actually test each feature on
        - a representative sample of those you support

# Having the "it won't look the same in all browsers" chat

- The "HD TV vs old black and white TV" is a good analogy
    - You wouldn't expect a TV show to look the same on both of those TVs so we
      don't expect the site/app to look "exactly the same" in all browsers

# http://caniuse.com

- it is your friend
- only useful if you know your browser support matrix first

# outline: 1px solid #bada55;

- browser dev tools are great these days but outline is still king for debugging
  layout issues
- add outline in various colors to the divs you are interested in
- outline is like border but it doesn't change the layout on the page so you can
  add it to blocks without disturbing your layout
- establish your browser support matrix at the start of the project

## Be careful with DRY CSS

- re-using styles is a great idea in theory
- a lot of words dedicated to this in blog posts etc.
- but ... "a little copying is better than a dependency"
- as in other code you are making a bet that "these things will change the same
  way in future"
- our layout tools aren't very good yet
- so things that superficially look very similar might actually require
  different HTML and CSS to achieve
    - i.e. it is hard to guess how similar two pieces of view are until you have
      actually built them!
    - so build them separately and only then bring them together if you think
      they will change the same way in future
    - the maintainability cost is high if you are wrong

# Don't add CSS you don't need

- it might seem like adding margin and padding to things "to be sure" is ok but
  it quickly obscures what your CSS is trying to achieve
- it would be like assigning variables that you don't use in a ruby method -
  very confusing!
- it is fine to try things when you are trying to achieve a particular layout
  but delete them if they don't work
    - browser dev tools is probably a better place to experiment than in your
      code
- every line of CSS should be there because you have proved to yourself that the
  site won't work without it

## Media queries

- decide whether you want to be "big screen default" or "small screen default"
- put them at the end of your module
- media queries are per-module - each module will require breaks in different
  places
    - thankfully nesting of media queries in SCSS makes this sane (otherwise we
      probably wouldn't do it)
    - other systems will pick some breakpoints and use them for the whole site -
      we don't have to do that because SCSS
