How does JS chain methods

```js
function a() {
    console.log('in a');
    return this;
}

function b(arguments) {
    console.log('in b');
    return this;
}

function c(arguments) {
    console.log('in c');
    return this;
}

a().b().c();
// in a
// in b
// in c
```

- JS evaluates chained methods from left to right

# The steps:

Evaluate a down to a value Find the 'b' property of that value evaluate b() down
to a value Find the c property of that value evaluate c() down to a value return
that value as the return value of the whole expression

```js
var x = get('story.json').then(f1).then(f2);
```

The steps

- evaluate 'story.json' (it evals to itself)
- evaulate get('story.json') down to a value (a promise)
- find the 'then' property of that value
- evaluate f1 to a value
- evaluate then(f1) to a value (a promise)
- find the 'then' property of that value
- evaluate f2
- evaluate then(f2) to a value (a promise)
- assign that value to x

```js
var p1 = get('story.json')

var p3 = p1.then(function (val1) {
  p
  });

var p2 = p1.then(f1).then(f2);

var p1 =
```
