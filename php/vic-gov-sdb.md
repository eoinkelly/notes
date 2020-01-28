# Vic gov SDP

https://www.vic.gov.au/what-single-digital-presence-offers

* Backend: Tide
    * a distribution of Drupal 8
    * seems to only install modeules created and maintained by the SDP project
* Bay: Their PaaS
  * based on https://stories.amazee.io/introducing-lagoon-amazee-ios-fully-open-source-docker-in-production-system-2be1f385fc2a
* Ripple
  * a design system
  * vue.js
  * a lib of reusable components

Options

1. host under vic.gov.au - no customisation required
2. vanilla
   1. a digital agency customises the **front-end**
      1. > The agency can change the interface style (including logo and colours) but cannot change its function. This means they can change the CSS but not the JavaScript.
3. Custom
   1. Use Bay and Tide. Use of Ripple is optional
   2. digital agency builds presentation layer and adds custom drupal modules as required
        * can use Ripple as a base if rquired
