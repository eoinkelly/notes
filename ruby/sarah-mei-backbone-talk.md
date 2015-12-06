Sarah Mei: Backbone & Rails talk
================================

* bb has no controllers
* what bb means by models, templates, views, controllers is different to what rails means

Backbone                rails
--------                --------
models/collections -->  models
templates          -->  views
views              -->  controllers

* A backbone view is a piece of code that controls a piece of the DOM


SM reckons JS usage in ralis goes from:
1. using the automagic rails js stuff
2. client wants stuff that this doesn't do well, so they move to raw jquery
3. that becomes a mess when there is a lot of it, so they move to a "page object" pattern
4. when you outgrow that you think about client side rendering (moustache & friends).

SM recommends not using rails js helpers in anything except very simple apps

page objects pattern
--------------------

* take all the JS separate it into unit testable functions and put is in a JS object scoped to the page being rendered
* this means you can unit test all the functions and the integration test can just check that your initialize() is being called
* pro: you can test it easier
* cons: you do a lot of boilerplate

rails dev want (and backbone provides)
* client side rendering
* model mirroring

SM sees 2 patterns in use:

pattern 1: greenfield
    * use rails just as api server
    * no server side rendering into HTML
    * a *lot* of work compared to old way
    * used a lot when people building mobile apps as well as site
    * she would consider something more opinianted than backbone for future green-field apps (backbone is maybe too flexible for this)

pattern 2: application in transition
    * an existing rails app that is rendering HTML server-side
    * backbone as frosting on the cake
    * stuff still rendered on the server, then backbone overlays it and takes over
    * she thinks that backbones sweet-spot is this "frosting" pattern as backbone is very flexible