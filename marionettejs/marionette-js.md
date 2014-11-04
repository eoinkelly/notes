# Marionette

@derekbailey designed the instance of `Backbone.Marionette.App` to be the thing you put in `App` global.


Where should you put


initializers are run when App.start() is called


```js
var App = Backbone.Marionette.Application();

// The options object you pass to start() is passed (and can be mutated) by all
// initializers and the startup event handlers

App.start(/*options*/)

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

load order options

1. use browserify and commonjs modules
2. we can make our globals more resilient to load order by doign DI (passing in any external references they need)


### Event aggretation

* you can turn any object in your system into an event aggregator if you choose
* is an implementation of `Backbone.Event`

Marionette provides 3 kinds of aggregator

1. `App.vent`
    * useful for _passively_ sharing information between parts of your app
    * you can pass a parameter and a context to
        App.vent.on('eventname', handlerFunc, contextToRunFuncIn)
2. `App.reqres`
    * request/response
3. `App.commands`

