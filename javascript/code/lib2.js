import { stateObj } from './lib.js';

export function mutateStateObj() {
  console.log('lib2.js: before mutate', stateObj); // {}
  stateObj.a = 1;
  console.log('lib2.js: after mutate', stateObj); // { a: 1 }
}
