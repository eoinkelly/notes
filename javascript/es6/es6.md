# new Javascript(); // ES6

* evolutionary approach to the language
* `git checkout js-master && git merge coffeescript`
* rebranded ES2015, attempting move to "release train" ala Chrome

# Ways to get ES6 features

1. babel.js (transpiler, currently best feature support anywhere)
2. io.js (much more support than node, still not near babel.js)
3. node 0.12 (distant 3rd)

More: [http://kangax.github.io/compat-table/es6/](http://kangax.github.io/compat-table/es6/)


# Modules

* Node `require` is a runtime function not syntax.
    * Works great for node but other contexts need to treat it as _syntax_ e.g. browserify and friends
    * i.e. they cannot _evaluate_ it to figure out where to find the resource
* ES6 has module _syntax_
    * ES6 does not specify _how_ modules are found and loaded
* Syntax similar but slightly different to Node






# Final thoughts

* New features will start becoming available (and appearing in your codebase!)
* Worth adding a build step where previously there was none?
* Similar trade-offs to coffeescript for babel e.g. not exactly same source in debugger
* things that are syntax transforms vs things that need runtime polyfill
* babel = most "future living". node stable = most "present living"
* Worth developing some sort of opinion now about how much to take on
* farewell coffeescript?

# How to dig deeper

* Bible: http://kangax.github.io/compat-table/es6/
* Playground: http://babeljs.io/repl/
* Good: http://rauchg.com/2015/ecmascript-6/


# Modules

* Language-level support for modules for component definition.
* Codifies patterns from popular JavaScript module loaders (AMD, CommonJS).
* Runtime behaviour defined by a host-defined _default loader_.
* Implicitly async model – no code executes until requested modules are available and processed.

* babel
    * will transpile to any of the usual module formats

# Babel

* top features
    * readable output
    * great docs including a REPL in page
    * will work on React.js code too
    * source maps
    * no runtime dependency (unless you want one)
    * browser support IE9+ (IE8 possible if you limit feature usage)

babel-node
    * wrapper around node that will precompile any loaded .es6 modules

Loose mode aka "faster but less compliant" mode
    * does not enable new transformers
    * tells some transformers to favour clean code over covering *all* edge cases
    * rationale
        * these edge cases are unlikely to appear
        * there is significant overhead in supporting them
        * code generated in loose mode will run faster and be more readable
    * can enable it on a transformer by transformer basis

playground support
    * for enabling "canary" transforms
    * enabling playground features also enables experimental
    * the stuff in here hasn't even necessairly made it to ECMA yet

experimental

"module formatter"
    * a transformer that takes in ES6 module syntax and converts it to other module syntax (common, amd, etc.)

## polyfill

babel includes a polyfill that includes

1. a custom regenerator runtime
    * created by facebook
    * https://github.com/facebook/regenerator
    * takes generator/yield syntax and turns it into runnable JS
    * there is a static transformer part and a runtime part
        * it is the runtime part included in this polyfill
2. core.js.
    * a "standard library" for JS
    * provides polyfills for all the various things that are built-in to modern JS

* The polyfill will emulate a full ES6 environment.
* This polyfill is automatically loaded when using babel-node and babel/register.

