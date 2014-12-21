
# Sources

* http://tomdale.net/2013/09/progressive-enhancement-is-dead/
* https://speakerdeck.com/garann/progressive-enhancement-for-js-apps
* http://tomdale.net/2013/09/maybe-progressive-enhancement-is-the-wrong-term/ (I agree with this)
* https://speakerdeck.com/garann/progressive-enhancement-for-js-apps
* http://danielmall.com/articles/progressive-enhancement/
* http://adactio.com/journal/6246/
* http://danielmall.com/articles/progressive-enhancement/

# What is a definition of progressive enhancement for JS app developers?

It is _not_ working with JS disabled.
is it about picking a baseline that includes JS ???

# What are the ways that PE is the same for both JS apps and web pages?

* HTML element support
    * polyfill as required

* CSS
    * App should work if CSS does not load or is disabled.
    * App should not break if ??? CSS features are not present

# What would the differences be between a fully PE compliant(TM) Ember app and a standard one?

During first load ...

* Must handle being offline/unable to communicate with the server
    * would have to cache all data locally, replay any changes on server when we get back online
    * seems manageable
    * probably something we have to do anyway

* Must handle JS not being available/turned off
    * start by delivering a server rendered start page and then boot the JS
    * if JS boot fails fall back to a server (HTML+CSS only) flow
    * this is a huge amount of work!
    * ? is this basically having 2 apps?
    * the user might enter your app on any page so has to work for all
        * probably not too hard given the boot process would be the same in all cases ???

* Must handle JS being there but slow
    * ???

* Must handle JS missing features
    * define a minimum set of features required
    * ? or define a set of supported browsers
    * polyfill as required

* Must handle some/all images not displaying
    * always add alt tags to content images
    * request appropriately sized image (JS could help here, otherwise default)

* Must work for screen-readers



# Tools

* https://github.com/zipfworks/ember-prerender
* https://prerender.io/
* https://github.com/rendrjs/rendr (server rendering of backbone apps)

