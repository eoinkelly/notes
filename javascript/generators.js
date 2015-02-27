
function* helloWorldGenerator() {
    yield 'hello';
    yield 'world';
}

var hw = helloWorldGenerator();
console.log(hw); // { next: [Function], throw: [Function], return: [Function] }
console.log(hw.next()); // prints { value: 'hello', done: false }
console.log(hw.next()); // prints { value: 'world', done: false }
console.log(hw.next()); // prints { value: undefined, done: true }
