// it seems test files are not modules but are commonjs?
import mainThing, { foo, greeting } from './foo.js';

test('some test', () => {
  console.log('default import:', mainThing());
  console.log('greeting:', greeting);
  console.log('foo:', foo());
  expect(1).toEqual(1);
});
