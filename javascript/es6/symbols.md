# Symbols

Uses

1.
2.

Pitfalls

```js
// Creating a symbol
// The value you pass Symbol() is a description of the symbol and is used when the symbol is turned into a string
const sym = Symbol('some-string'); // note no 'new' - Symbol() is not a constructor

//they do not automatically coerce to string but you can explicitly convert
sym.toString();
```

Symbols as property keys in an object are ignored by

Object.keys() Object.getOwnPropertyNames() for-in loop

Symbols are a new primitve type in JS

```js
typeof Symbol();
('symbol');
```

Every symbol value created by `Symbol()` is unique - each symbol has its own
identity GOTCHA: they are not at all like ruby symbols

Symbols as property keys

- symbols can be used as property keys in objects
- by exploiting the syntax JS has for using computed values as property keys

```
// you can use square brackets to compute a property key at runtime
{ ["a" + "b"]: 123 } // => { ab: 123 }

// similarly for symbols
{ [Symbol('ab')] = 123 } // => { Symbol(ab): 123 }

// You can even define function names at runtime
let thing = { ["do" + "Stuff"]() { return "hi" } }
thing.doStuff() // => "hi"

// and the same works for symbols (remembering that all symbol values are
// unique so you have to save the symbol value somewhere if you want to use it to
// retrieve a key later)

const FOO = Symbol()

let ob = {
    [FOO]() {
        return "hi from foo"
    }
}

ob[FOO]() // => "hi from foo"
```

JS Terminology

- property keys can be strings or symbols
- string valued property keys are called "property names"
- symbol valued property types are called "property symbols

```
Object.getOwnPropertyNames(ob)      // returns only string value keys
Object.getOwnPropertySymbols(ob)    // returns only symbol value keys
```

Use cases

- they suggest using symbol keys instead of the `_` prefix for object properties
  you wnt to kee private
    - this seems of slightly dubious value to me
- they are ideal as "meta level" keys of an object because they can never name
  clash with an ordinary key
- they don't even name clash with each other so you can have many "property
  symbols" with the same description in a single object
    - if a lib adds a symbol key to objects it has to
        1. store a ref to that symbol somewhere outside the object to later
           retrieve the property value
        2. is garuanteed that the key will be available in the object this seems
           pretty handy for things like Ember

```js
// works just fine
{
    name: "Eoin",
    [Symbol('name')]: "something else",
    [Symbol('name')]: "yet again"
}
```

JS itself uses the symbol returned by `Symbol.iterator` as a special key in
objects - if the key exists and is a function then the object is considered
"iterable" and that function will be used to walk across it

Coercion

- They only have implicit coercion into Boolean
- They can be explicitly coerced into String but not Number

Symbols cannot be represented in JSON

Aside: JSON.stringify() and JSON.parse() have an optional second arg which will
process the object before/after conversion. You can use these functions to
convert symbols into a string representation

Global symbol registry

- JS has the idea of "code realms" - each realm has its own global variables etc
- code can be executed in more than one realm
- example of realms: iframes on a page
- Strings, Number and other primitive types are passed by value so can cross
  realms easily but symbols are not
- JS gets around this via the "global symbol registry
- Note that `Symbol.iterator` is not in the GCR - it is built-in to JS itself

```
Symbol.for("some string") // register a symbol in the GSR
Symbol.keyFor("some string") // retrieve a symbol from the GSR
```
