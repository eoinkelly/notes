# Ember.Object

    Ember.Object = Ember.CoreObject + Ember.Observable

# Ember.Observable

- We shouldn't use observers in Ember if we can avoid them - use computed
  properties instead - see https://www.youtube.com/watch?v=vvZEddrClAQ
- Observers and computed properties achieve similar things

- `Ember.Observable` provides both observers and computed properties

## get()

- gets the value of a property
- provides a unified interface over
    1. `object.keyName` and `object[keyName]`
    1. computed properties
    1. the `unknownProperty` handler

```js
let Person = Ember.Object.extend({
    name: null,
    age: null,
    unknownProperty: function (propName) {
        // called when get() is called on unknown property
        console.log(propName);
        return 'hi from unknown';
    },
    greeting: Ember.computed('name', 'age', function (/* propName */) {
        return `${this.get('name')} of ${this.get('age')} is me`;
    })
});

let thing = Person.create({
    name: 'Eoin',
    age: 37
});

// get() can get normal JS properties, computed properties and unknownProperty
console.log(thing.get('name'));
console.log(thing.get('age'));
console.log(thing.get('notThere'));
console.log(thing.get('greeting'));
```

There are a number of handy variations

```
get('propName') // basic getter
getWithDefault('propName', defaultValue) // provide a default value if one is missing
getProperties('prop1', 'prop1' ...) // get multiple properties (takes multiple args
getProperties(['prop1', 'prop1' ...]) // get multiple properties (takes array arg)
```

## set()

Similar to normal JS setting but also provides support for

1. setUnknownProperty()
1. will trigger observables
    - note computed properties are NOT triggered when new data is set - they are
      lazy, observers are eager

there are many handy variations

```
set('propName') // basic setter
setProperties('prop1', 'prop1' ...) // set multiple properties (takes multiple args
setProperties(['prop1', 'prop1' ...]) // set multiple properties (takes array arg)
    * these are both set within a beginPropertyChanges() and endPropertyChanges() batch so observers are buffered
incrementProperty('propName', amountToIncrement)
decrementProperty('propName', amountToDecrement)
toggleProperty('propName') // toggle a boolean
```

## Misc methods from Observable

```
notifyPropertyChange

cacheFor
* allows you to inspect the value of a computed property without invoking it
* returns the cached value of a computed property if it exists

addObserver
removeObserver
```

## Observers

Observers: Ember.observer

- observers do not fire at object initialization
    - you can wrap the observer in an on('init' ...) call to get around this
- observers are eager, computed properties are lazy

TODO: show the two syntaxes for creating these

## Computed properties

Computed properties: Ember.computed

TODO: show the two syntaxes for creating these

# Ember.CoreObject

The parent class of all ember objects

## Cool feature: concatenation

Normally when you extend an object in JS the new one just overrides the old
(Backbone does this). Ember.CoreObject lets you specify some properties that
should be concatenated together rather than being overridden

- -- this is a bit unexpected so can complicate your model

ember uses it for arrays only AFAIK this works for arrays - what about

string number object bool

## Destruction of ember objects

You can mark an object for destruction and it will actually be destroyed when
the runloop gets around to it

```js
var e = Ember.Object.create({});
e.destroy(); // schedule desctruction
```

destroy()

1. sets the "isDestroying" flag immediately. Trying to get/set properties on the
   object after this will raise an exception.
2. removes metadata which effectively destroys observers and bindings
3. schedules `this.willDestroy` onto the 'actions' queue of the runloop
4. schedules `this._scheduledDestroy` onto the 'destroy' queue of the runloop

Reasons for destroy()

An object can be GC'd when all references to it are gone. destroy() removes all
bindings and observers immediately so the only references left should be the
ones in `this._scheduledDestroy` and whatever context created it.

This way of destroying an object lets you have bindings and observers without
going crazy

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

- `Object` is a constructor function
- so it has an object as a `prototype` property
- it links this object to the magic internal `__proto__` property of every
  "child" object it creates. Objects will lookup properties through this link if
  they fail to find them intenally
- except `Object.create(null)` has no prototype property (not it is not an empty
  it is missing!)
- The official version of `xx.__proto__` is `Object.getPrototypeOf(xx)`

```js
var xx = {};
xx.__proto__ === Object; // false
xx.__proto__ === Object.prototype; // true
Object.getPrototypeOf(xx) === Object.prototype; // true

xx instanceof Object; // true
```

`instanceof` does the same check as above but it is smart enough to search the
whole prototype chain - we can only search on level at a time without it.

```js
xx instanceof Object // true
// looks for Object.prototype *anywhere* in the prototype chain of xx
x

So a constructor function's prototype object is basically shared mutable state. You probably want to pretend that it is immutable for your sanity.


`prototype.constructor`
```
