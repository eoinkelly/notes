# ES 6 Modules syntax

- Can I use ES 6: http://kangax.github.io/compat-table/es6/
- Final syntax: http://www.2ality.com/2014/09/es6-modules-final.html

- By default nothing is exported from a file.
- You can rename on export and also on import
- You can re-export imported stuff

```js
// re-export everything from the other module
export * from 'lib/crypto';
```

# Named exports

```js
// lib.js
var fun1 = function () {};
var fun2 = function () {};
export { fun1, fun2 as funny };

// app.js
import { fun1, funny } from 'lib';
```

# Default export and import

- You can have a single default export from a file
- You can simultaneously have a default export and named exports

- the thing passed to `from` is called the _module ID_
    - the default interpretation is as a path relative to the importing module

```js
// lib.js
var fun1 = function () {};
export default fun1;

// app.js
import fun1 from '/path/to/lib';
```

# Not importing, just run for side effects

```js
// if you are not importing something from inside the module, you are importing the module
import 'blah'; // executes the file but doesn't import anything
```
