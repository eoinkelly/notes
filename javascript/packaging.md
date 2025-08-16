# What runs where

## Background: node ESM support

- For any file that Node.js tries to load, it will look for a package.json in
  that file’s folder, then that file’s parent folder and so on upwards until it
  reaches the root of the volume.
- node 13.2.0 was first version with ES module support (that wasn't behind a
  flag)
- node treats the following as ES modules:
    1. If the package.json has `type: "module"` then all its files are loaded as
       ES modules
    1. If the module's entry point filename ends in `.mjs`
    1. If you reference the module from an `import` statement
- everything else is treated as CommonJS
- `import` statements that load a CommonJS module can only load the default
  export
- You can be explicit about the "commonjs'ness" of a module:
    - Use `.cjs` filenames
    - Set `type: "commonjs"` in `package.json`
- It is a best practice for modules to specify their `type` in `package.json`
  now
- You must use the file extension with `import` unless the package has an
  `exports` defined for your import
- `exports` field in `package.json`
    - lets you explicitly say which paths in your module can be imported
      (whether via `require` or `import`)

        ```jsonc
        // package.json
        {
        "main": "./main-require.cjs",
        "exports": {
            "import": "./main-module.js",
            "require": "./main-require.cjs"
        },
        "type": "module"
        }


        // package.json
        {
        "main": "./main.js",
        "exports": {
            ".": "./main.js",
            "./feature": {
            "node": "./feature-node.js",
            "default": "./feature.js"
            }
        }
        }
        ```

    - successor to `main` (`main` supported in all versions of node but is more
      limited)
    - https://nodejs.org/api/packages.html#packages_conditional_exports

## Overview

In the context of a web application in Ruby/Python, we usually see:

- packages in `devDependencies`
    - build tools
    - intended to by run by node
    - typically uses commonjs but can be ESM depending on node support
- packages in `dependencies`
    - intended to eventually be run by the browser
    - ways that an npm package can get to the browser are detailed below

# Ways that you can put code intended for the browser into an npm package

1. Directly referencing the built file
    - you'd be using the npm package as just a way to get the file(s) on disk
    - Could happen in theory e.g. your HTML file has
      `<script href="./node_modules/thing/index.js">...` but seems unlikely
1. Via a build tool
    - Some build tool copys and optionally transforms the file(s) into a file
      for the browser to download and execute

Input types to the copy+transform tool

- UMD packaged module
- ES6 module
- CommonJS export
- other???

## Background: UMD

https://github.com/umdjs/umd

A pattern to create a packaging format compatible with:

1. CommonJS
1. AMD
1. Browser globals

You can choose which combination of those formats you want to hit

## Webpack

Webpack is my build tool

it does diff things when you reference files in diff ways

From a pack file, webpack does the fllowing

- commonjs requires (`require("@rails/ujs")`:
    1. finds the file on disk
    1. transpiles with babel (???)
    1. bundle the transpiled file into the pack (wrapping in the webpacker
       loader code)
    1. Replace the call to `require()` with something like:
        ```js
        @require('trix'); // becomes
        __webpack_require__(/*! "trix" */ "./node_modules/trix/dist/trix.js");
        ```
- imported relative paths (`import "../things/mything.js"`):
    1. finds the file on disk
    1. transpiles with babel
    1. bundle the transpiled file into the pack (wrapping in the webpacker
       loader code)
    1. Replace the call to `import` with something like:
        ```js
        import '../jquery/rams'; // becomes
        /* harmony import */ var _jquery_rams__WEBPACK_IMPORTED_MODULE_2__ =
            __webpack_require__(
                /*! ../jquery/rams */ './app/frontend/jquery/rams.js'
            );
        ```
- imported npm packages (`import "bootstrap"`)
    1. finds the file on disk
    1. transpiles with babel
    1. bundle the transpiled file into the pack (wrapping in the webpacker
       loader code)
    1. Replace the call to `import` with something like:
        ```js
        import $ from 'jquery'; // becomes
        /* harmony import */ var jquery__WEBPACK_IMPORTED_MODULE_0__ =
            __webpack_require__(
                /*! jquery */ './node_modules/jquery/dist/jquery.js'
            );
        ```

so after webpack is finished, it doesn't really matter whether you pulled in the
code via require() or `import` because it ends up the same (modulo the usual
diffs in semantics between require and import)

require() evaluates the module code at that point so does `import` the result of
the evaluation is cached so the evaluation is only run once per page load

I think my mental model of `import` was that it pulled symbols in but didn't
actually run code I think i was thinking the import happened at parse time and
was resolved much earlier a better mental model is to think of it as running a
function which returns a hash which we have special syntax for picking the bits
we want from.

import = run the code in the module (aka function) and return whatever it
exports import => go run the code in this module now like a function and return
it's exports to me

any module import/require from a webpack pack file is (aside from babel
transpiling) copied in as is
