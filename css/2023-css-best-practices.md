# CSS best practices for 2023

- [CSS best practices for 2023](#css-best-practices-for-2023)
  - [Usable in 2023 (now)](#usable-in-2023-now)
    - [Logical properties](#logical-properties)
    - [Dynamic viewport units dvh dvw](#dynamic-viewport-units-dvh-dvw)
    - [Container size queries](#container-size-queries)
    - [Cascade layers](#cascade-layers)
    - [oklch colors](#oklch-colors)
    - [dialog element](#dialog-element)
    - [is() pseudo class](#is-pseudo-class)
  - [Usable in 2024](#usable-in-2024)
    - [:has() selector](#has-selector)
    - [CSS subgrid](#css-subgrid)
    - [@scope](#scope)
    - [nesting](#nesting)
  - [No firm timeline for usability](#no-firm-timeline-for-usability)
    - [grid-template-rows: masonry](#grid-template-rows-masonry)
    - [Container style queries](#container-style-queries)
    - [relative color syntax](#relative-color-syntax)
    - [view transitions](#view-transitions)
    - [popover attribute](#popover-attribute)

## Usable in 2023 (now)

### Logical properties

* https://caniuse.com/css-logical-props
    * ready for use now (Safari 15+ supports it)
* stylelint plugin to auto-convert https://github.com/csstools/stylelint-use-logical

### Dynamic viewport units dvh dvw

* https://caniuse.com/viewport-unit-variants
  * ready for use (Safari 15.4+ )
* https://web.dev/viewport-units/

### Container size queries

* https://caniuse.com/css-container-queries
* https://caniuse.com/css-container-query-units
* ready for use (safari 16+)

### Cascade layers

* https://caniuse.com/css-cascade-layers
  * usable now
* unclear how useful they will be

### oklch colors

* https://caniuse.com/mdn-css_types_color_oklch
    * usable now
* oklch can display the p3 color space
* can use `@supports` to see if the browser supports the `oklch()` function

what happens on non p3 monitors?

### dialog element

* https://caniuse.com/dialog
    * usable now

### is() pseudo class

* https://caniuse.com/css-matches-pseudo

## Usable in 2024

### :has() selector

* https://caniuse.com/css-has
* in everythign but firefox (behind flag in firefox)

### CSS subgrid

* https://caniuse.com/css-subgrid
  * chrome 117+ (stable as of 2023-07-07 is 114)
  * safari 16+
  * many firefox versions
* As of 2023-07-07 not ready for prod but should be by start of 2024

### @scope

* caniuse: https://github.com/Fyrd/caniuse/pull/6760/files (PR not landed yet)
* spec: https://www.w3.org/TR/css-cascade-6/#scoped-styles
* browser support:
    * as of 2023-07-04
        * chrome 117+
        * safari: nothing shipped but they [intend to support](https://github.com/WebKit/standards-positions/issues/13)
        * firefox: nothing shipped
* status: ignore for 2023

### nesting

* browsers: https://caniuse.com/css-nesting
    * as of 2023-07-04
        * chrome 112+ (latest stable 114)
        * edge 112+ (latest stable 114)
        * firefox 116+ (latest stable 116)
        * safari: 16.5 (latest 16.5)
* status: support in latest versions only, not ready yet but could be by year end?

## No firm timeline for usability

### grid-template-rows: masonry

* https://caniuse.com/mdn-css_properties_grid-template-rows_masonry
  * Safari 17 only
* Not nearly ready

### Container style queries

* https://caniuse.com/css-container-queries-style
  * not nearly ready yet

### relative color syntax

* https://caniuse.com/css-relative-colors
  * safari only for now

### view transitions

* https://caniuse.com/view-transitions
  * chrome only and behind a flag for now
  * easy to make a progressive enhancement but no point until it's shipping on by default in some browser


### popover attribute

* https://caniuse.com/mdn-api_htmlelement_popover
    * chrome only and safari 17
