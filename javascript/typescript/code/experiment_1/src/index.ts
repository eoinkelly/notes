// this is the consumer

// each dependency is a diff style

// import { drive } from "./option1";
// drive();

// import ALL the thigns exported by option1 making them available under the namespace in this module
// ??? what to call this namespace of "functions and static data which pertain to cars"
//      we don't want to overlap with the type name (in ruby these would be the same)
import * as carHelpers from './option1';
import { create as createCar, drive as driveCar } from './option1';
import type { Car } from './option1';
import { timeStamp } from 'node:console';

// this creates an "elixir alike" pattern where modules are namespaces of functions and data is always passed into them
let myCar: Car = carHelpers.create();
carHelpers.drive(myCar);

let myCar2 = createCar();
driveCar(myCar2);

class Thing {
  private a: number;
  private b: number;

  constructor(a: number, b: number) {
    console.log(`Running Thing constructor`);
    this.a = a;
    this.b = b;
    console.log(this.privateInstanceMethod());
  }

  // public static members (functions or variables)

  // members are public by default
  static publicByDefault = 'publicByDefault';
  public static publicByExplicit = 'publicByExplicit';

  static publicMethod() {
    return 'publicMethod';
  }

  private static privateMethod() {
    return 'privateMethod';
  }

  private static privateByExplicit = 'privateByExplicit';

  // protected is also available

  publicInstanceMethod() {
    return 'publicInstanceMethod';
  }

  private privateInstanceMethod() {
    return 'privateInstanceMethod';
  }
}

console.log(Thing.publicByDefault);
console.log(Thing.publicByExplicit);
// console.log(Thing.privateByExplicit); // does not compile

let t = new Thing(22, 33);
console.log(t);
console.log(t.publicInstanceMethod());
// console.log(t.privateInstanceMethod()); // does not compile

// Conclusion: Typescript classes have everything I need to design with

// Q: what about using ES modules instead of classes for code organisation
// is that more idiomatic in JS? if so, why?

/*
using modules the consumer has a lot more choice about how they consume the individual things you export
    is that totally a good thing?

if i org my code using a class, then the consumer has to consume it as such and use it as such

is it sensible to mimic a class using a module

*/

// Q: Are wildcard imports bad? They seem better to me than not (less typing, explicit namespacing)
import * as mimic from './mimicClass';

console.log(mimic.publicStaticIsh());
let mm = mimic.construct(55, 33);
mimic.greet(mm);

console.log(mimic.someDict);
mimic.someDict['hello'] = 'there';
console.log(mimic.someDict);

import { someDict as aliasDict } from './mimicClass';
console.log('alias:', aliasDict);
