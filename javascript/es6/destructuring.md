
can also do default values
```js
/*
* can be used in any location that receive data
  * left side of assignment
  */



const ob = { name: "eoin", age: 37 };

const { name: n, age: a } = ob; // you can destructure a full object
// the above is shorthand for
// const n = ob.name;
// const a = ob.age;

console.log(n, a);

const { name: n2 } = ob; // you can destructure just part of an object
console.log(n2);

// there is a shortcut for destructuring a value into a variable with the same name as its prop
// in the object
const { name, age } = ob;
// const name = ob.name
// const age = ob.age
console.log(name, age);

// destructuring can be used to pick apart nested data structures
const nesty = { name: "eoin", address: { stree: "blah", suburb: "newlands" }};
const { address: {suburb}} = nesty;
console.log(suburb);

// Arrays can also be destructured

const things = [1,2,3,4,5];

const [a1, b1] = things;
console.log(a1, b1);

// you can capture "the rest" with "...foo"
const [head, ...rest] = things;
console.log("head,rest", head, rest);
```
