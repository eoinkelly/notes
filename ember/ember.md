# The Ember boot process

* Ember.Application.create() will automatically schedule a boot at
  DOMContentLoaded (after initializers have run)
* Implications:
    * We can defer that scheduled boot by calling `App.deferReadiness()` anytime
      before DOMContentLoaded

```js
// to begin with _readinessDeferrals =  1
App.reset() // sets flag = 1
App._initialize() // decrements it

visit() helper does something with decrementing it
```

* If `App.testing` is true then Ember increments it _in an initializer_.
* Implications:
    * `App.setupForTesting()` indirectly increments the counter by scheduling an
      increment in an initiailizer.
