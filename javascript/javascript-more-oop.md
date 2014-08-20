
Object literals can only appear inside *expressions* in JS
----------------------------------------------------------

Object literal and block statements have *exactly* the same syntax in JS

// JS considers this a statement
{foo: "bar"} // => "bar"

// We can force JS to consider it as an expression using parentheses
({foo: "bar"}) // => Object

// JS considers this an object literal because it is expecting an expression
var x = {foo: "bar"} // => undefined

var a = b = 12; // => undefined
c = d = 12 // => 12

so assignment returns the RHS but the var returns undefined

Prototypal inheritance provides no contract or garuantees - there are no static garuantees that object X has a given set of properties


Adding properties to an ES5 object
----------------------------------

var x = Object.create(null);

Object.defineProperty(x, 'name', { value: 'Eoin', writable: true, configurable: true, enumerable: true });
Object.defineProperty(x, 'name', { value: 'Eoin', writable: true }); // configurable and enumerable set to false by default
Object.defineProperty(x, 'age', { value: 33, writable: false, configurable: true, enumerable: true });
x.height = 183; // sets writable, configurable, enumerable to true
x.name = 'Amelia'; // works
x.age = 31; // fails silently

var getter = function () {
  return this.first_name + ' ' + this.last_name;
}
var setter = function (first, last) {
  this.first_name = first;
  this.last_name = last;
}
Object.defineProperty(x,
  'fullname',
  {
    get: getter,
    set: setter,
    writable: true,
    configurable: true,
    enumerable: true
  }
);

* Note the crazy syntax for defining a getter/setter in an object literal:
var foo = {
  first_name: 'eoin'
  last_name: 'kelly'
  get name() {
    return this.first_name + this.last_name
  },
  set name(new_name {
    // new_name = ...
  }
};


The object argument above is a 'descriptor'. There are two types:
1. data descriptor
  * contains
    - value = the value of the property
    - writable
      * can the value of the property be changed (only applies to data descriptors)
      * defaults to false if not specified in Object.defineProperty
    - configurable
      * can the property be changed or removed
      * defaults to false if not specified in Object.defineProperty
    - enumerable
      * should this property be listed in a loop through the properties of the object
      * defaults to false if not specified in Object.defineProperty
2. accessor descriptor
  * contains
    - get ()
    - set (new_value)
  * when accessor descriptors are not set they default to 'undefined'

Accessing properties of an object
---------------------------------

All property names are eventually converted to a string so all these are equivalent :
x[1]
x[[1]]
x["1"]
x[y] // where y = 1;

* dot notation can also be used but isn't as flexible about what the name is (it must be a valid JS IdentifierName)

Remove a property from an object
--------------------------------

delete x.age; // => true
delete x.notHere; // => true

* note that it seems to return true even if the property didn't already exist in chrome ????
* more info at http://perfectionkills.com/understanding-delete/

Getting all the properties of an object
---------------------------------------

method 1:
  Object.keys(foo); // returns a list of "own" properties in foo that are marked "enumerable"
    * this will be the same or a subset of Object.getOwnPropertyNames

method 2:
  Object.getOwnPropertyNames(foo) // returns list of "own" properties in foo

method 3:
  for (p in foo) { // only goes through *enumerable properties of an object and it's ancestors
    if (foo.hasOwnProperty(p)) { // filter out results from our ancestors (found by following [[Prototype]])
      console.log(p);
    }
  }
  * note that 'foo' must inherit from Object.prototype for this to work - it won't work if you did Object.create(null)

this
----

this = a reference to the object that the funciton is being applied to
  it means you can apply a function to *any* object that meets the requirements of the function, regardless of how the object has been constructed.

"javascript functions are generics"
  * not sure what this means but it seems to imply that 'this' is decided at runtime

4 Ways of resolving 'this'
1. directly
  * foo() // sets this to the global object (window in browser, global in node)
2. as a method
  * obj.foo() // sets this to 'obj'
3. explicitly applied
  * foo.call(myThis, p1, p2);
  * foo.apply(myThis, params) // WARNING: can be a lot slower than calling function other ways
4. as a constructor
  * watch out - constructors can return whatever they want - if you don't explicitly return it will return the constructoed object but you can override this.

instanceof
==========

  myObject instanceof someConstructor

tests whether an myObject has someConstructor.prototype anywhere in its prototype chain

binding methods
===============

general gist is that you take an object and a function and you return another function that always has this set to object

var adder = function (a) {
  return this + b;
}

var addThree = adder.bind(3)

Mixins
======

A mixin is a parentless object (empty [[Prototype]] link)