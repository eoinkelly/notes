# ES6 import and export

- Sources
    - https://exploringjs.com/es6/ch_modules.html
    - https://exploringjs.com/js/book/ch_modules.html#use-cases-for-top-level-await
- Module imports are hoisted (internally moved to the beginning of the current
  scope).
    - Therefore, it doesn't matter where you mention them in a module
- The ES6 module standard has two parts:
    1. Declarative syntax (for importing and exporting)
    2. Programmatic loader API: to configure how modules are loaded and to
       conditionally load modules
- The imports of an ES6 module are read-only views on the exported entities.
    - they are a kind of read-only pointer to the actual value in memory the
      module holds all its exported values in memory all modules which import
      those valeus get a read-only pointer to the exported value
    - Imports as views have the following advantages:
        1. They enable cyclic dependencies, even for unqualified imports.
        2. Qualified and unqualified imports work the same way (they are both
           indirections).
        3. You can split code into multiple modules and it will continue to work
           (as long as you don't try to change the values of imports).
    - That means that the connections to variables declared inside module bodies
      remain live
    - **imports are read-only views but you can still mutate exported objects**
      -- The binding is read-only so you cannot re-assign an imported symbol to
      another variable. But you can mutate the object e.g. CRUD fields in it,
      add/remove to arrays etc.
- Modules are singletons.
    - Even if a module is imported multiple times, only a single “instance” of
      it exists.
- the structure of ES6 modules is static, you can’t conditionally import or
  export things
- The body of a module is executed when it is first imported by something else
- The default export is actually just a named export with the special name
  default. That is, the following two statements are equivalent:

    ```js
    import { default as foo } from 'lib';
    import foo from 'lib';
    ```

    Similarly, the following two modules have the same default export:

    ```js
    //------ module1.js ------
    export default function foo() {} // function declaration!

    //------ module2.js ------
    function foo() {}
    export { foo as default };
    ```

- Imports work differently in CommonJS and ES6:
    - In CommonJS, imports are copies of exported values.
    - In ES6, imports are live read-only views on exported values.
- In contrast to CommonJS, imports are views on exported values. In other words,
  every import is a live connection to the exported data. Imports are
  read-only: - Unqualified imports (`import x from 'foo'`) are like
  const-declared variables. - The properties of a module object foo
  (`import * as foo from 'foo'`) are like the properties of a frozen object.
- Being "live" values means that if the source module changes the value then
  importers will see the new value

## Modules are sometimes loaded sync, sometimes async (depending on environment)

Node loads modules sync. Browser loads them async.

    TODO: verify this

## imports as views under the hood

Exports are managed via the data structure "export entry". All export entries
(except those for re-exports) have the following two names:

1. Local name: is the name under which the export is stored inside the module.
2. Export name: is the name that importing modules need to use to access the
   export.

After you have imported an entity, that entity is always accessed via a pointer
that has the two components module and local name. In other words, that pointer
refers to a binding (the storage space of a variable) inside a module.

Let's examine the export names and local names created by various kinds of
exporting. The following table (adapted from the ES6 spec) gives an overview.

| Statement                      | Local name  | Export name |
| ------------------------------ | ----------- | ----------- |
| export {v};                    | 'v'         | 'v'         |
| export {v as x};               | 'v'         | 'x'         |
| export const v = 123;          | 'v'         | 'v'         |
| export function f() {}         | 'f'         | 'f'         |
| export default function f() {} | 'f'         | 'default'   |
| export default function () {}  | '_default_' | 'default'   |
| export default 123;            | '_default_' | 'default'   |

## Import statements must always be at the top level of modules.

That means that you can’t nest them inside if statements, functions, etc.
Therefore, you have to use the programmatic loader API if you want to load a
module conditionally or on demand:

```js
if (Math.random()) {
    System.import('some_module').then(some_module => {
        // Use some_module
    });
}
```

Also remember they are hoisted to the top of the file.

## Can I use destructuring in an import statement?

No you can't. The import statement only looks like destructuring, but is
completely different (static, imports are views, etc.)

## Further reading

This section gives pointers into the ECMAScript 2015 (ES6) language
specification.

Managing imports:

- [CreateImportBinding()](http://www.ecma-international.org/ecma-262/6.0/#sec-createimportbinding)
  creates local bindings for imports.
- [GetBindingValue()](http://www.ecma-international.org/ecma-262/6.0/#sec-module-environment-records-getbindingvalue-n-s)
  is used to access them.
- [ModuleDeclarationInstantiation()](http://www.ecma-international.org/ecma-262/6.0/#sec-moduledeclarationinstantiation)
  sets up the environment of a module (compare:
  [FunctionDeclarationInstantiation()](http://www.ecma-international.org/ecma-262/6.0/#sec-functiondeclarationinstantiation),
  [BlockDeclarationInstantiation()](http://www.ecma-international.org/ecma-262/6.0/#sec-blockdeclarationinstantiation)).

The export names and local names created by the various kinds of exports are
shown in [table 42](http://www.ecma-international.org/ecma-262/6.0/#table-42) in
the section
“[Source Text Module Records](http://www.ecma-international.org/ecma-262/6.0/#sec-source-text-module-records)”.
The section
“[Static Semantics: ExportEntries](http://www.ecma-international.org/ecma-262/6.0/#sec-exports-static-semantics-exportentries)”
has more details. You can see that export entries are set up statically (before
evaluating the module), evaluating export statements is described in the section
“[Runtime Semantics: Evaluation](http://www.ecma-international.org/ecma-262/6.0/#sec-exports-runtime-semantics-evaluation)”.

# Questions

does all module resolution happen before execution or during it in node? modules
are "executed async" what does this mean?
