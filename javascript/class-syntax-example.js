/*

* This is a sample file for me to drop into a project to test whether all the class features I care about are available
* All the below tested in babel 7.15.0
* Many of the class features landed in ES2022 (due to be official in Jun 2022)
* many of it's proposals are stage4 now (Feb 2022) so have been implemented in babel and browsers

Terminology:

* classes contain "fields" (ivars)
* classes contain "prototype methods" (functions, instance methods)
* classes contain "static methods" (class methods)
*
* classes are sugar for creating functions e.g. `class Foo {` is `function Foo(<constructor args>)...`
* class can be a "class declaration" or assigned to a variable as a "class expression"

*/

// anonymous class expression
const MyThing = class {};
// MyThing2.name == ""

// named class expression
const MyThing2 = class MyThing2 {};
// MyThing2.name == "Mything2"

class Animal {
  #privateInstanceField = 'unknown'; // private instance field (with optional initial value)
  publicInstanceField = 'blah'; // public field (with optional initial value)
  static #privateStaticField = 'x'; // static private field
  static publicClassField = 'x'; // static public field

  #type = 'whatever';

  // constructor function
  constructor(name, type) {
    // this == the instance of the class

    // you can create class fields here without declaring them
    // public fields do not need to be declared (but it's good to do so I guess)
    this.name = name;

    // set private fields
    // private fields must be declared before you can assign to them
    this.#type = type;

    // can use super keyword in here to call the constructor of our parent
  }

  // getter for custom getting
  get name() {
    return this.name;
  }

  // setter for custom setting
  set name(newName) {
    this.name = newName;
  }

  // public instance method
  // equivalent to Animal.prototype.speak = function() ...
  speak() {
    console.log(this.noise);
  }

  // static public property
  static numBones = 111;

  // static public method
  static doStaticStuff() {
    // this == undefined in here
    console.log('static');
  }

  // private static method
  static #doSecretStuff() {
    // ...
  }

  // field declaration syntax
  // you can declare class fields outside of an instance method or constructur
  // fields can be declared with a alue
  age = 12;
  // const age = 12; // does not work, you can't choose let/const/var  for class fields
  disposition;

  // private field
  #privateAge = 33;

  // static initialization block
  // Not supported by babel 7.15.0
  // I'm not sure what I would use these for tbh
  // static {
  // }
}

const thing = new Animal('joe', 'bear');

thing.speak();

console.log(Animal.numBones, Animal.doStaticStuff());

console.log(thing);

// super is a keyword
// can be used to call methods on parent classes

/*eslint max-classes-per-file: "off"*/

class Dog extends Animal {
  // properties
  constructor(name) {
    super(name, 'dog');
  }
}

const woofy = new Dog('spot');
console.log(woofy);
