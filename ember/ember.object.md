
# Ember.CoreObject

Cool features

## Cool feature: concatenation

Normally when you extend an object in JS the new one just overrides the old (Backbone does this). Ember.CoreObject lets you specify some properties that should be concatenated together rather than being overridden

* - this is a bit unexpected so can complicate your model - should document properly

ember uses it for arrays only AFAIK
this works for arrays - what about

string
number
object
bool

## Destruction

Destroy with

```js
var e = Ember.Object.create({});

e.destroy();
```


destroy()

1. sets the "isDestroying" flag immediately. Trying to get/set properties on the object after this will raise an exception.
2. removes metadata which effectively destroys observers and bindings
3. schedules `this.willDestroy` onto the 'actions' queue of the runloop
3. schedules `this._scheduledDestroy` onto the 'destroy' queue of the runloop

Consequences:

An object can be GC'd when all references to it are gone. destroy() removes all bindings and observers immediately so the only references left should be the ones in `this._scheduledDestroy` and whatever context created it.

This way of destroying an object lets you have bindings and observers without going crazy



## Metadata

What are these things in an ember object

```
__super__

__ember1417371339599: null
__ember_meta__: Object__
nextSuper: undefined
`__proto__: Class`
    * the standard JS link to the constructor function that created this object

```

# Aside Javascript inheritance refresher


* `Object` is a constructor function
* so it has an object as a `prototype` property
* it links this object to the magic internal `__proto__` property of every "child" object it creates. Objects will lookup properties through this link if they fail to find them intenally
* except `Object.create(null)` has no prototype property (not it is not an empty it is missing!)
* The official version of `xx.__proto__` is `Object.getPrototypeOf(xx)`

```js
var xx = {};
xx.__proto__ === Object // false
xx.__proto__ === Object.prototype // true
Object.getPrototypeOf(xx) === Object.prototype // true

xx instanceof Object // true
```

`instanceof` does the same check as above but it is smart enough to search the whole prototype chain - we can only search on level at a time without it.

```js
xx instanceof Object // true
// looks for Object.prototype *anywhere* in the prototype chain of xx
x

So a constructor function's prototype object is basically shared mutable state. You probably want to pretend that it is immutable for your sanity.


`prototype.constructor`


```
