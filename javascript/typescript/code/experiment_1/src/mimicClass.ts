/**
 * A module which mimics a class
 *
 * * ++ the consumer can import a subset of my public interface if they want
 * * ++ the consumer can rename the things I export if they want to (??? dubious value maybe sometimes bad?)
 * * -- the consumer has to pass the instances I create back into my "instance" methods
 *    * that's really what makes an instance method an "instance method", the passing of an instance as the first arg
 * * -- If i need instances with instance methods, there isn't really much upside to this approach vs just creating a real class
 * * ?? it's a more "functional" approach, with all data being explicitly passed around
 * * ?? inheritance would be bit awkward (maybe not a bad thing tho)
 */

/*
Quesiton: where to put the types?

option: put type in the module that "owns" it
  * ++ fairly logical place to look for it
  * -- you don't get to find all types for an app in just one place

option: put all types for the app in a single file
  * ?? how does it scale to a whole app?
  * ++ you can `import type` from just a single file

option: create a .d.ts files

typescript can automatically pull types from other npm packages
  so the only confusion is where to put the types in an app which isn't a lib

should i treat subfolders in the app as if they are npm packages?
  they don't have a package.json to guide typescript

types that are used in one file are easy
types used in all files are easy
where to put types used in many but not all files?

code is organised into features
thing.ts
thing.types.ts


Each lambda is a "feature"/package
so all the types in the feature are sharable within the feature
=> have a single types.ts

*/

// public static method
export const publicStaticIsh = (): void => {
  console.log("publicStaticIsh");
};

// private static method
export const privateStaticIsh = (): void => {
  console.log("privateStaticIsh");
};

interface Thing {
  a: number;
  b: number;
}

export const construct = (a: number, b: number): Thing => {
  console.log(a, b);
  return {
    a: a,
    b: b,
  };
};

// public instance method
// -- awkward having to pass in self
export const greet = (self: Thing): void => {
  console.log(generateGreeting(self), self.a, self.b);
};

// private instance method
export const generateGreeting = (self: Thing): string => {
  return "Greetings from: ";
};

// public class variable (constant)

export const SOME_VAL = 12235;

// public class variable (mutable)
interface SomeDict {
  [name: string]: string;
}
export let someDict: SomeDict = {};
