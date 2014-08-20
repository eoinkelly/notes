# Ember Runloop

* If you want to work with rendered content put it on the ‘afterRender’ queue
* NB: You probably don't ever need Ember.run.next()
* There is a plan for browsers to expose their runloop to JS via an API
* Ember has an event dispatcher that responds to ember actions
* It automatically runs in production but not in testing
* It is possible to insert more queues into ember runloop
* The run loop is not always running
* While the browser is executing the runloop it
* The runloop is ember's response to an event - it does all the updating and
  propagates the consequences of that event throughout the system. Then it is
  done and will finish and wait around for the next event

## When does it run

* It does not run all the time
* It is a discrete operation that runs when
    * use mouse/keyboard events
    * ember data ajax responses
    * others ???

## How does it run?

* Loop through queues, performing each callback
* After each callback see if computations schedules in previous queue
* If so start the whole process over from the the first non-empty queue
* Continue until all queues empty


## How do I wrap code in a run loop?

You should wrap your asynch code in a run loop because

* more efficient
* hard to debug/test without it
* potentially non-determniistic behaviour

```
Ember.run(object, function (obj) {
    // Ember.begin() // pseudocode

    // ... custom code ...

    // Ember.run.end() // pseudocode
    // Ember now propagates all changes that result from our custom code
    // throughout the system
}]:]:);
```
* You give `Ember.run()` a function that will trigger some events (and maybe do
  other stuff). Ember will notice those events and do all the stuff required
* You can expect that when the runloop has finished that *all* the bound data in
  the app will have updated.

This will
* immediately run the callback
* execute the run loop algorithm


How can i get visiblity into when the ember run loop is running?
```
Ember.run // the run loop
```

## What is the difference between observers and bindings in ember?

* Binding propagation happens in the `sync` queue of the RunLoop
* Observers are fired immediately when the watched property changes - they are
  **not** scheduled on a RunLoop queue.


This means that if an observer and binding are watching the same property, the
Observer will **always** be called first.

Aside: setTimeout(0) is ~3-4ms on modern browsers, ~15ms on mobile and older browsers


scheduleOnce() vs. Ember.run.once ???

Ember.run.next used to be recommended way to do stuff after DOM rendering - it
has been superceded by Ember.run.scheduleOnce('afterRender' ...)

## Resources

* [http://alexmatchneer.com/blog/ember_run_loop_talk/#/intro](Good slides)
* [http://alexmatchneer.com/blog/2013/01/12/everything-you-never-wanted-to-know-about-the-ember-run-loop/](detailed blog post)
* [https://github.com/ebryn/backburner.js/](The library that implements the runloop)
* [https://machty.s3.amazonaws.com/ember-run-loop-visual/index.html](Awesome queue visualisation)


Used to

* batch work
    * wait for a no. of tasks to need doing before we start work
    * the run loop tends to batch *similar* kinds of work
        * similar means
            * reads from the DOM
            * writes to the DOM
* order
    * control the order that tasks are exwcuted
* reorder
    * execute tasks in a different order than they arrived

work in a way that is most effective and efficient
    ? efficient how?
    * efficent in terms of the browser having to do layouts and paints

Ember's computed properties dependencies mean that a single property update can
result in numerous changes to the DOM - the run loops stops this from being
terrible
* The run loop waits for all properties to settle down before rendering (similar to Angular)

* The run loop processes queues in the order they appear in:
```
Ember.run.queues // ["sync", "actions", "routerTransitions", "render", "afterRender", "destroy"]
```

* sync
    * resolve bindings
* actions
    * schedule actions after bindings have resolved
* routerTransitions
* render
    * views render themselves into the DOM
* afterRender
    * for devs to schedule actionss after changes to DOM
* destroy
    * Ember.Object destruction happens here

I think jobs are executed on the queue in the order they are added

When will I need to work with queues

* Integrating third party JS that include some sort of asynchronous callbacks
  e.g.
    * AJAX callbacks
    * DOM update and event callbacks
    * Websocket callbacks
    * `postMessage`   and `messageChannel` event handlers


```
// Schedule `method` to run one time in `queue` of the current RunLoop
Ember.run.scheduleOnce(queue, target, method, args*)
```


How do I start a run loop?

```javascript
$('foo').click(function () {
    Ember.run(function() { // begin loop

        // schedule jobs on the loop here

    }); // end loop, jobs are flushed and executed
    // at this point you can be sure that whatever stuff you scheduled has been
    run and the DOM has been updated. This is why Ember.run() is handy for
    testing.
});

```

Some of Ember's test helpers are promises that wait for the run loop to empty
before resolving.
This means that if you run code outside the run loop, these test helpers will
resolve too early which might cause your test to fail when it should not.

To fix this you can disable loop autoruns

```
Ember.testing = true; // disables the automatic runloop

// Now you can manually schedule asynchronous operations to run in a one off
// runloop via Ember.run

```
