# Scoped CSS aka Nesting

- Support 2021-04-27: Basically none, behind a flag on Chrome and Firefox
- https://drafts.csswg.org/css-nesting/
- PostCSS supports an early draft of it which probably won't match
- it cannot concatenate parent selectors like SCSS and PostCSS can e.g.
  `&--blah` or `&__other`
    - but Sass team kinda wish that feature didn't exist either
- uses `:is()` under the hood with the specificity rules that come with `:is()`
    - you cannot get it to use `:where()` under the hood
