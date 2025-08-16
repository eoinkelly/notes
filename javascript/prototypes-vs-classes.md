# The case against inheritance in JS

# https://medium.com/javascript-scene/the-two-pillars-of-javascript-ee6f3281e7f3

OLOO (Objects linking to other objects) vs Class based inheritance

Consider trying to use an ordinary constructor as a factory function

```js
function Foo() {
    // ignore `this`
    // return an arbitrary object

    return {
        name: 'a',
        age: 24
    };
}

// f will have no __proto__ link to Foo.prototype - it will link to Object.prototype instead
var f = new Foo();
```

He contends that JS does not need constructor functions at all. Any function can
be a factory for objects

- no `new` danger
- no special casing of `this` within the factory function because of `new`

Problems with OO inheritance:

- > Every OO design taxonomy I have seen was eventually wrong
- > You wanted a banana but you got a gorilla holding a banana and the entire
  > the jungle
    - you usually get more than you want when you inherit from a parent
- There is very tight coupling between the child and parent class
- The _duplications by necessity_ problem of OO design - ???

He says there is causation between the problems of OO design and codebases
needing rewrites but doesn't really back it up

## Alternative but similar ideas

1. Doug Crockford does not use `new` or `this` at all in his JS now - he has
   moved to an entirely functional style of code re-use
    - pure, stateless functions and plain data objects
    -   - if you are creating lots (100k+ objects) and need it to be performant
          then delegating calls to methods can save a lot of manual memory
          management - ???
2. use modules as an alternative to inheritance e.g. node style modules
3. simply clone objects by copying properties from one to another e.g.
   `_.extend(...)`
    - this is another form of prototypal inheritance - the source is called an
      _exemplar prototype_ and the process of cloning is called _concatenative
      inheritance_.

He quotes from the GoF book:

> “Program to an interface, not an implementation,” and “favor object
> composition over class inheritance.”

He contends that in inheritance the child class programs to the _implementation_
of the parent not its _interface_.

- child has to know what parent does if it is going to call `super` - ??? is
  that really true?

He states 3 advantages of prototypal inheritance

1. Simpler: no more incorrect class taxonomies
2. More flexible: you can switch from creating new instances of objects to
   re-using objects from a pool to proxies (???)
3. More powerful and expressive: You can inherit from multiple ancestors and
   inherit private state

# Define prototype

A prototype is a working sample.

There are 3 ypes of prototypes:

1. Delegate prototype
    - If you don't find what you need in the object, delegate to another object
    - "delegating" property lookup ot another object
    - You get "flyweight" objects for free - each property is only stored once
      in memory
    - JS does this type

    ```js
    var personProto = {
        name: 'eoin',
        speak: function() { ... }
    }

    var p1 = Object.create(personProto);
    // make 1000 more person objects

    // can modify prototype here and all 1000 person objects will get it
    proto.walk = function() { ... }
    ```

2. Cloning/contatenation
    - Good for default state
    - Good for mixins

    ```js
    var personProto = {
        name: 'eoin',
        speak: function() { ... }
    }

    var p1 = _.extend({}, personProto, { name: 'Colm' });
    ```

3. Functional inheritance (not really a type of prototype but achieves similar
   goals)
    - Great for function encapsulation/privacy
    - Great for functional mixins
    - Replaces:
        - constructor functions
        - super()
        - init functions

    ```js
    // implement a model with functional inheritnace
    var model = function() {
        var attrs = {}; // private

        this.get = function(name) {
            return attrs[name];
        }

        this.set = function(name, value) {
            attrs[name] = value;
            this.trigger('change', ... )
        }

        _.extend(this, Backbone.Events);

        // We use this function to decorate `this` with properties in various ways
    }

    var foo = {};
    model.call(foo).set('name', 'foo');
    // now foo is mutated and has all the stuff that model added to it

    foo.get('name'); // 'foo'
    ```

The 3 types of prototypal inheritance combined let you do anything classes could
do.

Factory functions

- similar to constructors but don't use `this` in the same way
- mix and match all 3 types of prototype
- you pass any of the 3 types of prototype in as `this` via `.call()` and
  `.apply()`

## Stampit library

What are we trying to achieve with inheritance

- Delegate methods to something else
- get some default values for our state
- have private functionality that is not exposed to other objects in the system

- `.methods()` for delgates
- `.state()` for cloning/contatenation
    - you should not have mutable state stored on the prototype
    - prototype can still provide state defaults and just copy them over
- `.enclose()` for functional/data privacy

All of these return a factory function which you can

1. call directly to get the object
2. compose with `.compose()`

TODO: look into this

# http://davidwalsh.name/javascript-objects

He sees JS prototypal stuff as "behaviour delegation" and sees it as an
orthogonal system to inheritance - he does not believe that delegation is just a
"dynamic inheritance"

We can think of inheritance as a "flattening copy" action down the class chain
into the object:

- whatever is farther down the chain will win if there is a name conflict.
- The compiler will also sort out any calls to super as a special case - perhaps
  we can visualise those as a "rename then copy"

In any case the compiler builds a method lookup table that resolves every method
call.

I'm not sure I buy his statements about how different JS is to OO e.g. in ruby
an object walks up its ancestor tree looking for the method - this seems similar
to the JS delegation ???

## Do we really make new types in JS

Not really.

- A constructor function does not make something that `typeof` will see as
  different
- (i.e. will not change the internal `[[Class]]` property that
  `Object.toString()` would report
- you can use `instanceof` to check if an object has a particular function as
  the `constructor` property of any object on its `prototype` chain but that
  isn't really a type either.

## Mixins

They circument the prototypal inheritance chain by copying values (or references
for objects/functions) into the target.

## The oddness of .constructor

> the only time a .constructor property is added to an object is when that
> object is the default .prototype attached to a declared function, as is the
> case of Foo(). When objects are created via new Fn() or Object.create(..)
> calls, those objects don't get a .constructor added to them.

> Let me say that again: an object created by a constructor doesn't actually get
> a .constructor property to point to which constructor it was created by. This
> is an extremely common misconception.

> we have to change how we think of what the .constructor property means. It
> does not mean "the constructor this object was created by". It actually means
> "the constructor which creates any objects that end up getting [[Prototype]]
> linked to this object." Subtle but super important difference to get straight.

# Questions

- Have I used an inheritance heirarchy anywhere in my JS work? Things I do use

- Ember.Object
- Backbone `_.extend`
    - EE reckons BB extend does create parent-child inheritance herirarchy

# What does `_.extend()` do?

```js
var s1 = {
    name: 'Eoin'
};
var s2 = {
    address: 'New Zealand'
};
var dest = {};

_.extend(dest, s1, s2);
```

It copies the ownProperties from each of the source objects into the destination

It is different from `_.clone()` (which creates a shallow copy) in that ???
`_.clone()`

- does a shallow copy (lodash has an option to do deep copy)

# Aside: kinds of polymorphism

Types of polymorphism:

1. Ad-hoc polymorphism
    - special branching logic for each type
2. Parametric polymorphism
    - parameters satisfy generic requirements so only one function body required
