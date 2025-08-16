## for...of

- syntax designed to work with _iterable_ objects
    - iterable objects are
        - iterators created by generator functions
        - built-in iterables
        - objects implementing the iterable protocol
- many JS built-in objects are iterable e.g Array, String, Map, Set, NodeList

```js
// Basic example
// *************
const things = [1, 2, 3, 4];

for (const thing of things) {
    console.log(thing);
}

// Early returns
// *************
for (const thing of things) {
    // trigger an early finish of the iterator with:
    // break
    // throw
    // return
    // note: these will all close the interable so you cannot re-use it after that
}

// Generator example
// *************
function* myGenerator() {
    yield 1;
    yield 2;
    yield 3;
}

for (const x of myGenerator()) {
    console.log(x);
}

// iterable protocol example
// *************************

const myObj = {
    [Symbol.iterator]() {
        return {
            i: 0,
            next() {
                if (this.i < 3) {
                    return { value: this.i++, done: false };
                }
                return { value: undefined, done: true };
            }
        };
    }
};

for (const a of myObj) {
    console.log(x);
}
```

## for await...of

- An async version of `for...of`

## for...in

- iterates over the **enumerable properties** of an object **in an arbitrary
  order**
- can give surprising results if you are expecting to just get the keys in an
  object iself or the values in an array
- You can use `for...in` on all JS objects but if the object supports it, then
  you usually want `for...of`

```js
Object.prototype.customOnObject = 'custom on Object';
Array.prototype.customOnArray = 'custom on Array';

let things = [11, 12, 13, 14];
things.foo = 'bar';

for (const i in things) {
    console.log('property name:', i, ', property value:', things[i]);
}
// property name: 0 , property value: 11
// property name: 1 , property value: 12
// property name: 2 , property value: 13
// property name: 3 , property value: 14
// property name: foo , property value: bar
// property name: customOnArray , property value: custom on Array
// property name: customOnObject , property value: custom on Object

for (const i in things) {
    // filter out properties from higher up in the prototype chain
    if (things.hasOwnProperty(i)) {
        console.log('property name:', i, ', property value:', things[i]);
    }
}
// property name: 0 , property value: 11
// property name: 1 , property value: 12
// property name: 2 , property value: 13
// property name: 3 , property value: 14
// property name: foo , property value: bar
```

## forEach

- `Array` and `Map` have a `forEach` method which does what you would expect.

## for

- old-school C style for loop

```js
let things = [1, 2, 3, 5];

for (let i = 0, len = things.length; i < len; i++) {
    console.log(things[i]);
}
// 1
// 2
// 3
// 5
```
