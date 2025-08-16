import { inc, dec, counter } from './statey';

test('test stateful module transipling', () => {
  console.log('Counter:', counter);
  inc();
  console.log('Counter:', counter);

  expect(1).toEqual(1);
});
