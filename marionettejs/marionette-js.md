# Marionette

@derekbailey designed the instance of `Backbone.Marionette.App` to be the thing
you put in `App` global.

## Startup

Initializers are run when App.start() is called

```js
var App = Backbone.Marionette.Application();

// The options object you pass to start() is passed (and can be mutated) by all
// initializers and the startup event handlers

// Kick things off
App.start(/*options*/)

// Schedule an initializer
App.addInitializer(function (/*options*/) {
    // options is same object passed to start()
    // QUESTION: they seem to add instance globals here?
});

// Fires before any initializer is run
App.on('initialize:before', function (/*options*/) {
    // options.foo = "some new value";
});

// initializers run now

// Fired after all initializers have finished
App.on('initialize:after', function (/*options*/) {
});

// Kicks off the initializers
App.on('start', function (/*options*/) {
    Backbone.History.start(); // <-- seems to be recommended place to do this
});
```

Initializers (different to `initialize`)

* `initialize` is called immediately when the object is created but the _initializers_ are only called when the app is started.
* initializers are garuanteed to run - if you add one after the app has started it will run immediately
* The options object you pass to `start()` is passed to all these event handlers and all initializers.
    * They can mutate it

Events fired at application startup are fired in this order:

1. before:start
2. initialize:before
3. (all initializers run)
4. initialize:after
5. start

You can hook into these with `.on('start'..` or `onStart(..` but the former seems cleaner

    QUESTION: where to add instance globals?

    QUESTION: should app.start be called in document.ready ?

### Event aggregation

* you can turn any object in your system into an event aggregator if you choose
* is an implementation of `Backbone.Event`

Marionette provides 3 kinds of aggregator

1. `App.vent`
    * useful for _passively_ sharing information between parts of your app
    * you can pass a parameter and a context to
        App.vent.on('eventname', handlerFunc, contextToRunFuncIn)
2. `App.reqres`
    * request/response
    * ask a question and get an answer without knowing who answers
3. `App.commands`
    * get something done without knowing who does it.

```js
App.reqres.setHandler('questionName', function(arg){ ... }, context);
var response = App.reqres.request('questionName', 'somearg');


App.commands.setHandler('commandName', function(arg){ ... }, context);
App.commands.execute('commandName', arg); // <-- no meaningful return value
```

The default event "channel" name is `global` but you can change this when you createthe instance of the application object

```js
var App = Backbone.Marionette.Application({ channelName: 'everybody' });
```

You can access the channel using

```js
var channel = Backbone.Wreqr.radio.channel('global');
channel.vent
channel.reqres
```

## Marionette.Object

* `initialize` is called immediately after the object is initialized and is
  passed the same options as the constructor got.
* `marob.getOption('foo')`
    * look for `foo` as a key in the `marob.options` and if there is not one it will return `marob.foo`

```js
var ObFactory = Marionette.Object.extend({
    initialize: function (/* options */) {
    }
});

var thing = new ObFactory({ .. });
```

* has a `destroy` method that will
    * unbind all the events (backbone events not DOM events) directly attached to the instance
    * you can customise destruction by listening to `before:destroy` event

## Helper functions

Just some interesting bits from http://marionettejs.com/docs/marionette.functions.html

* When binding events you can trigger mutliple handlers by separating their names by a space
```js
Marionette.View.extend({
    modelEvents: {
        "before:destroy": "doThing1 doThing2", // multiple handlers as string
        "blah:blah": someFunc // function name is also legal
    }
});
```

* `actsAsCollection` will mixin underscore collection methods into the given
  Object or Function e.g. you can call `map` directly on the object rather than
  `_.map(ob, ...)`

* Marionette defaults to using the jQuery deferred. You can replace this by setting `Backbone.$.Deferred` but I don't know how viable that really is.

## TemplateCache

* Default behaviour is to load the template from the DOM given the selector in the view e.g. `#my-template`. This is then compiled with `_` templates and then cached in `Backbone.Marionette.TemplateCache`

* the cache has methods
    * `get('#some-template')`
    * `clear()` to clear all
    * `clear('#some-template)` to clear specific templates

## Marionette.Region

* Jobs a Region does:
    * Be the interface between a View instance and the visible DOM
    * Manage the attach/detach of View instances from points in the live DOM
* Those DOM attachment points must exist and be given to the Region when it is instantiated.
    * The DOM will be queried for the provided attachment point!
* They manage an area of DOM on the page. They alos manage part of the DOM tree
* The Application instance is a region manager - has lots of sugar for defining regions
* You can sub-class built-in `Marionette.Region` class and use that instead
* stores the currently attached view in `this.currentView`

* `empty()`
    * _shut down_ any view in the region. This means:
        1. trigger the 'before:destroy' event on the view
        2. call `view.destroy()` (on Marionette views) or `view.remove()` (raw Backbone views)
        3. delete the reference to the view in `this.currentView`
        4. trigger the 'empty' event on the view
* `show(newView)`
    1. trigger `before:swapOut` on existing view if any
    1. call `empty()` if `newView` is different to `currentView` and we don't have preventDestroy option set
    * renders the given view
    * does nothing if the region already contains an instance of that view but you can force it.
    * automatically renders that view. it will also destroy any view that was already there before replacing it.
* `reset()`
    * calls `empty()` and then deleted our cached reference to `el` - this forces us to query the DOM for el again the next time.
    * destroy view and cached reference to el DOM element (forces el to be queried from DOM next time you use region)
* `hasView()`
    * returns boolean that says whether region is currently showing a view or not
* `attachView(view)`
    * simply sets `this.currentView` to the provided view
* `this.currentView`
    * returns a refernce to the currently attached view (null if there isn't one)


Note: that if a Region is going to `show` a view then it will also `render` it - no exceptions

### show() in detail

1. Region `before:swapOut`
2. empty()
3. attach our `empty()` to newView's `destroy` event in case somebody else kills it
4. newView.render()
5. Region: `before:swap`
6. Region: `before:show`
7. newView: `before:show`
8. attachHtml()
9. Region: swapOut
10. Make `this.currentView` point to newView
11. Region: `swap`
12. Region: `show`
13. newView: `show`

* The region owns that space in the DOM, the view just lives there
* There is a 1:1 relationship between Region:View
* The Region DOM element stays put while the View DOM element gets created/destroyed along with the view lifecycle

## Marionette.RegionManager

* Manages a collection of regions (CRUDs them)
* Has a few events that fire before/after some of the CRUD operations
* Not really intended for direct use.
* `Application` and `LayoutView` implement RegionManager - probably most useful to use those instead.

## Routers

It is recommended that you divide your controller objects into smaller pieces of related functionality and have multiple routers / controllers, instead of just one giant router and controller.

if `onRoute()` exists in your router instance it will be called with the name, path, and arguments of the route whenever a route is changed.

## View

* `close()` will clean-up the things marionette setup in the background - it will not clean up your custom stuff - you should used `onClose()` for that




# fish app specific

TODO:
    overrider renderer/templatecache thign
    http://marionettejs.com/docs/marionette.renderer.html

we could load the factories under a "classes" namespace

Questions


load order options

1. use browserify and commonjs modules
2. we can make our globals more resilient to load order by doign DI (passing in any external references they need)

* backbone views _always_ have an `el` if `tagName`, `className` etc. are not
  specified it is an empty div.
* the backbone-view el is filled out by its render() function but that does
  **not** automatically insert it into the DOM.
    * In Backbone you do that manually, in Marionette a Region does it for you.
    * render() will change only the visible DOM if you set el to be something
      that is already attached to the visible DOM tree
    * There is a slight inbalance in that Backbone views do not have any
      "insert into visible dom tree" functionality - they do have a "remove
      from DOM tree" functionality

# Questions

    What is the most efficient way to load compiled handlebars templates in a marionette app?

    What is the most sensible organisation of the app using globals?

    Do we need global references to instances of marionette objects? is that an anti-pattern?

    Does marionette need to wait for DomContentLoaded to create instances of objects?

    Is it bad to have things in DOM that are not mapped to marionette objects?
