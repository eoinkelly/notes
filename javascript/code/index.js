import { stateObj } from "./lib.js";
import { mutateStateObj } from "./lib2.js";

// Fails
// stateObj = { a: 1 }; // TypeError: Assignment to constant variable.

// This works fine
// console.log(stateObj); // {}
// stateObj.a = 1;
// console.log(stateObj); // { a: 1 }

console.log("index.js: before", stateObj); // {}
mutateStateObj();
console.log("index.js: after", stateObj); // {}
