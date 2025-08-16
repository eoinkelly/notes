# Web components

## Pros/cons

- Pro: will work for many years to come, no framework dependency
- Pro: Can be upgraded without touching framework(e.g. React). React upgrades
  will not require any changes to the web components
- Con: not what devs are used to. Might need to learn new ways of solving
  problems
- Con: we haven't lived with them. Just speculation for now
- Con: for now, requires a wrapper component for React
- Con: no way to SSR
- ???: New features will be added very slowly unlike React which can move faster
- Pro: might mean that some apps can defer needing React until they **really**
  need it e.g. Rails+Turbo
- Con: much smaller ecosystem of 3rd party components
    - ??? How many of those components solve problems React creates
- ???: What happens if you need to coordinate between components on the page - a
  framework can really help ehre
- ???: perf vs react
- ???: harder to build because no vdom?
- ???: use them vs stimulus components?
    - ???: Stimulus controller could achieve almost the same thing assuming you
      only want light DOM - is this accurate?
    - WC Pro: could more easily re-use on other projects on other stacks
    - ???: is WC only worth it if you are re-using?

Possible use cases

1. App is server rendered and has relatively simple component needs
1. App is react rendered but some key leaf components are written in WC to make
   them more future proof.
    - Could we have a "WC if possible, otherwise RC" approach?

## Bluffers facts:

- General consensus is that you need a framework on top

## Questions

- do you need a design system first to make best use
-

## Sources

- https://daverupert.com/2023/07/why-not-webcomponents/
- https://web.dev/learn/html/template/
- https://www.matuzo.at/blog/2023/pros-and-cons-of-shadow-dom/

Web component frameworks

- https://lit.dev
    - by Google
    - part of polymer
    - apparently fairly simple
    - more a lib than a framework
- https://stenciljs.com
    - by Ionic
    - apparently a lot more magical and featureful than lit
    - has a VDOM, does tree shaking loading of components on a page
    - https://www.abeautifulsite.net/posts/moving-from-stencil-to-lit-element/
- https://hybrids.js.org
- Salesforce web components
    - https://developer.salesforce.com/docs/component-library/documentation/en/lwc
    - Has a big catalog of example components but they all look quite
      "salesforcey" so probably not super re-usable

## Frameworks which use web components

- https://rocket.modern-web.dev
- https://enhance.dev
- https://www.11ty.dev/docs/languages/webc/

## Web component libraries

- https://www.webcomponents.org/
- https://developer.salesforce.com/docs/component-library/overview/components
