## Forms in Angular

- Forms in angular are created with either ngForm or the FORM element (ngForm
  can be nested, FORM elements cannot)
- They both create an instance of a FormController
    - this means you can turn any element into an "angular form"
- Forms in angular have a different purpose than they do in regular HTML
    - They aren't really for submitting data to the server. Instead they do
      validation of inputted data
    - Collecting data from the user for the client-side angular app to use (this
      may include sending it on the network)
- Angular will disable the default submit action of a FORM element unless you
  explicitly specify the 'action' attribute.
- If the name attribute is specified on the form, the controller is published to
  the current scope using that name e.g.
    ```
    <form name="foo">
    ```
    will result in a `FormController` object available within the controller as
    `$scope.foo`

### Form validity

- Angular forms know whether they are valid and expose this info in their
  '$valid' attribute
- Angular Forms know whether they are valid or not. A form is valid if
    1. all its child forms are valid
    2. all its child input elements are valid

They also set CSS classes on the FORM element

- ng-dirty == the form has been modified by the user (not 100% sure this is the
  whole story)
- ng-pristine == the form has **not** been modified by the user (not 100% sure
  this is the whole story)
- ng-valid == the form is valid
- ng-invalid == the form is invalid (one or more of it's child INPUTs or child
  ngForms is invalid)

It also sets CSS classes on INPUT/FORM elements to indicate whether they
pass/fail specific validations

To query whether a form itself is valid or not, Angular exposes some attributes
on the FormController object:

```javascript
// <form name="eoinForm"> ... </form>
$scope.eoinForm.$valid;
```

Each INPUT element within an angular form knows whether it is currently valid

```javascript
// <form name="eoinForm">
//    <input name="firstName" type="text" />
// </form>
$scope.eoinForm.firstName.$valid;
```

Similar to rails, Angular puts form errors in a hash with the name attribute of
the INPUT/FORM as key and the error message as value.

```javascript
$scope.eoinForm.firstName.$error;
```

### Submitting a FORM

Since Angular disabled the default submitting behaviour (sending direclty to the
server is probably not what we want), we can set the ngSubmit attribute on the
FORM element to tell Angular what code to run when the user submits the form

```
<form name="eoinForm" ngSubmit="doIt()"
</form>

<!-- OR -->

<form name="eoinForm">
  <input type="submit" name="doIt()" />
</form>

<!-- Bad idea (form will be double handled by Angular):
<form name="eoinForm" ngSubmit="doIt()">
  <input type="submit" name="doIt()" />
</form>
-->
```

You can also put an ngClick directive on an INPUT element with `type="submit"`.
Be careful to only use one or the form will be **double-handled**.

## Controllers

Angular controllers thave two jobs

1. put initial values into the models exposed in the $scope
2. add UI specific functions to the $scope for the view to use

## $scope

The scope object is like an instance of a presenter passed to a rails view - it
has all the data values and behaviour that the view needs. In a way the
controller is just a constructor/factory for the $scope object

Angular controllers are just regular JS functions - they don't extend anything.
The scope object is very similar to the ViewModel in MVVM

Angular creates a root scope $rootScope when the application is bootstrapped ? I
think it is tied to the DOM element that ng-app directive appears on

As well as $rootScope, some other directives create new scopes e.g.

- ng-controller
- ng-repeat

These child scopes have a link to their parent via their `$parent` attribute -
this lets them for a tree that parallels the DOM

What does "create a new scope" mean? It means angular calls `Scope.$new()' - I
guess Scope is a scope factory

## Models

Angular does not give you any help with models. As far as it is concerned, any
data value (primitive, array, object) assigned to a $scope is a model.

This could be a great thing - it forces you to not weld your domain modesl to
the angular framework - will this make it easier to avoid the rails problem?

### ng-repeat

ng-repeat directive turns the element it is on into a repeating element i.e.
within a UL, you should put ng-repeat on the LI elements

- it creates a new scope so angular adds the `ng-scope` CSS class
- it has data bindings so angular adds the `ng-binding` CSS class
- angular puts a HTML comment at the end of each iteration
- angular duplicates the element that ng-repeat is set on, including the
  ng-repeat directive itself and any other attributes
- ng-repeat does not create a scope to wrap the list of scopes it creates

### Writing to a variable from a parent scope

child scope inherit all the properties of the parent scope - this follows the
usual prototype rules of inheritance in JS i.e. when we try to read a property,
it will first try the current scope, then the parent, then the grandparent etc.

A consequence of this is that you if a lower scope shadows a variable from a
parent scope it then you can't write to it by just using the varaible name. You
can write to it by useing the $parent property e.g. $parent.foo = 12;

The book recommends not using $parent too much as it welds your app to the
current structure of the DOM (just like jQuery does)

A better solution is to make the variable a property of an object - this way you
can modify it from any scope and it will work as expected e.g. ng-init="foo =
{bar: 12} This is better as it does not make assumptions about the DOM tree

"You should always have a dot in the expression provided to ng-model"

### Scope Events

- Scopes form a heirarchy tree.
- Any node on the tree can emit events.
- Events can have payloads
- The $rootScope is the root of this tree
- Events can be emitted either up or down (not both)
    - $scope.$emit() // sends event up from current scope to all parents (ending
      in $rootScope)
    - $scope.$broadcast() // sends events down from the current scope to all
      child scopes
    - $scope.$on('event-name', function(event, ...other args...) { ... }x5 ;
        - other args is the event payload (changes depending on the type of
          event
        - can call event.preventDefault() to prevent the default aciton for both
          emitted (upwards) and broadcast (downwards) events
        - can call event.stopPropagation() only when handling events that have
          been emitted (upwards) by lower scopes - why ???
- Use cases for events
    - Angular broadcasts (downwards) the
      '$locationChangeSuccess' event from the **$rootScope instance\*\* whenever
      the URL in the location bar changes

- Only 3 events emitted (upwards) by Angular
    - $includeContentRequested
    - $includeContentLoaded
    - $viewContentLoaded ??? what do these do?
- Only 7 events broadcast (downwards) by Angular
    - $locationChangeStart
    - $locationChangeSuccess
    - $routeChangeStart
    - $routeChangeSuccess
    - $routeChangeError
    - $routeUpdate
    - $destroy TODO what do all these events mean?

Usually scopes are created and destroyed automatically by Angular

- implies you can't control GC as precisely as you could with Backbone

but you can manually manage scopes:

- Scope.$new() // create a scope
- Scope.$destroy() // destroy a scope

### Angular uses the DOM as template (not HTML)

- Angular is particulary sensitive to badly formed HTML syntax and will probably
  give weird errors
- Angular never reads HTML - it waits for the browser to make the DOM and then
  uses **the live DOM tree** as it's template
- Angular lets the browser do it's template parsing!

- Angular JS code (except for directives) shouldn't have any references to DOM
  code

You can add data attributes to your $scope from the controller or from the
template (via ng-model) AFAIK the only way to map INPUT, TEXTAREA elements to an
attribute of $scope is with ng-model so you can't avoid having that in your
template???

- The template says _what_ to do
- The directive knows _how_ to do it
- Angular decides _when_ to do it

## Modules

You can _create_ and _retrieve_ modules with the `angular.module()` function

```
var modInstance = angular.module('someName', []); // create 'someName' module
var myNameMod = angular.module('someName'); // retrieve a reference to an existing model
```

- A module acts as a container for other managed objects
- The `ng-app` directive is given the name of a module.
- Modules can contain any kind of object, not just angular specific ones

In angular you can declarativly express dependencies between modules

- Pros
    - you can swap collaborators as you need them
    - you are not responsible for creating or destroying (managing life cycle)
      of collaborators - angular does it for you - this means less code but also
      less control I guess
        - if you object doesn't create or destroy any of its collaborators, it
          has less jobs
        - it makes _all_ the collaborators **swappable** (hence easier to
          change)
    - it lets you unit test much easier. The ability to swap collaborators is
      very important for testability

To take advantage of angular DI system, you need to tell it about your code by
registering your code in modules.

Angular modules do not store instances of the objects your code needs - they
store factories/recipes/constructors that can create these instances

- The $provide service allows us to register our recipes
- The recipes are then interpreted by the $injector service (which creates and
  wires up their dependencies too)
- objects created from a recipe are called _services_

NB Angular will only interpret a given recipe **once** during the applications
life cycle - there will only every be one instance of each service object!!!

Consequences

- there is only one of each controller, filter, directive, service that you
  register
- if I want to use Angular DI/modules for my own code, I need to remember that
  the constructor function I provide will only be run **one time** during the
  lifetime of the app.
- Angular services are singletons!!!
- ? does this imply that services are not useful for models?

### 7 ways of registering an object with Angular DI system

1. moduleInstance.value('name', func)
    - registered by value() cannot have dependencies
    - func must be a constructor
2. moduleInstance.service()
    - not used very often apparently
    - lets you make services
    - the function you provide must be a constructor (decorates `this`, no
      explicit return)
3. moduleInstance.factory('name', )
    - allows to register any arbitrary object-creating code i.e. the function
      does not have to be a constructor - it can return **anything**
    - the function you pass can be used as the module pattern (it can have
      "private" variables, and return it's public interface)
    - factory is the most common way of registing code with the DI system
4. moduleInstance.constant('name', value)
    - lets you register a constant that can be declared as a dependency by other
      modules
    - You could put constants within the function you pass to factory but this
      is better because
        - they can be swapped out for tests
        - you can re-use the service across many applications
        - con: they don't have default values - the client app has to specify
          **every** constant
5. moduleInstance.provider('name', function)
    - all of the other registration methods are sugar for this one
    - it must return an object that contains a `$get` property which is a
      factory function
    - a provider is an object that embeds factory function in it's $get property
    - it can also expose other functions as properties and have hidden data so
      you can have a default configuration that gets overridden by the functions
      it exposes.
        - it is the most verbose but most flexibly way of registering
6. moduleInstance.config(func(fooProvider))
    - note there is no name parameter, only a function
    - allows you to call functions on the provider object before it's $get
      property is invoked to create the service
    - these functions get run during the config phase (they can change the
      recipe)
    - gets passed in an instance of a provider (decided by the parameter name)
    - Two ways of registering config functions for angular modules:

    ```
    angular.module('my-foo-mod', [], function (myServiceProvider) {
      // I am **the** config function because I am the third argument (implicit - less obvious)
      // I am the only one - this is less flexible
      // the parameter passed in here is a reference to the **provider** that creates the service instances
      // angular uses its argument parsing magic to know find the provider for 'myService'
      myServiceProvider.setMaxLen(5);
    });

    // This form is prefered as it is more explicit and flexible
    angular.module('my-foo-mod', [])
      .config(function () {
        // I am a config function
      })
      .config(function () {
        // so am I
      })
    ```

    - within the config() function we are calling things in the public interface
      of the **provider** object (which contains the factory object in `$get`).
      This is like changing the recipe, not creating the service using the
      original recipe and then tweaking it
    - Angular uses it's magic "parse the function text" trick to find the right
      provider using the naming convention fooProvider (the provider for foo)

7. moduleInstance.run(func(...))
    - note there is no name parameter, only a function
    - code that should be invoked at the run phase of the module

Function objects registered as modules get an `$inject` attribute which points
to an array of strings representing their dependencies - this is what all the
sugar ways of declaring dependencies does.

## Inspecting Angular from the console

How do I inspect the angular objects in console?

```javascript
angular.element($0).scope(); // or just type  $scope with batarang installed
```

If you change value of some model on
$scope and want to have this change reflected in the running application, you need to call $scope.$apply()
after making the change.

```
example here
```

??? You can almost do it for services:

```
$('body').injector().get('myMod'); // works iff you Angular is using full jQuery
```

## Module lifecycle

Angular module has two phases

1. configuration phase
    - all recipes are collected & configured
    - providers can be configured (using modInstance.config()) here
    - config() callbacks are run here
2. run phase
    - any post-instantiation logic is run here
    - the equivalent of a main method in other languages
    - a module can have multiple run blocks
    - run() callbacks are run here
    - you can use run callbacks to add variables to the $rootScope

Angular modules can be combined into other modules

- groups of related services can be combined into re-usable modules
- the top level (application) module can just declare its dependencies on what
  it needs

Angular has both a services namespace **and** a module heirarchy. All services
are global singletons so the modules (a heirarchy of service creation recipes)
eventually creates a flat services namespace.

- services can declare dependencies on other services, values, constants (they
  don't care what modules these are in however)
- modules (each of which can contain multiple services) can declare dependencies
  on each other

## Service visiblity

- All services are combined into a global namespace so they can all depend on
  each other no matter what module they are defined in.
    - This means that the module heirarchy is flattened out
        - The module herirarchy is still worthwhile because it helps for
          organisating code and testing
    - You can override services by defining ones with the same name that are
      "closer" to the service that needs them.
    - Services defined in modules that are closer to the root of the **module**
      heirarchy will override those in child modules
    - There is currently no way of having a service that is private to a module
    - There is currently no way of restricting the visibility of a service

## Angular & jQuery

- Angular has it's own subset of jquery called jqlite
- it will use jQuery if it finds it on the page (jquery has to be loaded
  **before** angular is)
- Using jQUeryUI stuff is not encouraged - it might work but directives are
  prefered

## Testing

- Use Jasmine
    - Angular code itself is tested in Jasmine
    - so the angualr guys have written some mock objects and jasmine extenstions
      to play nice with jasmine

## Folder structure

- they like to organise by feature - this feels odd to me but could be nice
- they believe that angular controllers and their view partials tend to evolve
  together so having them in the same folder is handy
- they recommend
    - organise most files by feature but cross cutting concerns get organised by
      type
    - test files have same name as the file they test
    - unit tests end in `.spec.js`
    - partials end in `.tpl.html`

- They strongly recommend one module per file
    -   - it is easier to load just the module you need for testing
    -   - you can find the module on the filesystem directly

    in angular, provider == recipe each recipe can have many forms (factory,
    provider, service, variable) but each recipe(provider) creates exactly one
    running service
    - how does this work with not having all your controllers in one file ???

They recommend using the chaining syntax to add stuff to modules

- pros
    - no temp variable (for module name) created (possibly in global scope if
      you don't use a closure)
    - you don't repeat the module name heaps which makes it easier to rename it
- cons
    - you pretty much need to stick to the one module per file thing.
    - ? this might help keep your code more modular?

`END CHAPTER 2`

## protractor video

These are notes from a presentation by Jim Lavin on protractor

- protractor lets you use selenium grid to run tests in parallel and load-test
  your servers
- you can run your tests against **remote** servers so you can test production
  or staging, not just development
- protractor does not use karma at all
- you can use jasmine or mocha to wite tests in protractor
- Jim lavin recommends installing protractor locally, not globally
- you can tell selenium what browser capbilities you need e.g. screen size
- you don't have to require jasmine or webdriver explicitly - projractor does it
  for you
- use ptor as the name of the progractor driver object object
    ```
    var ptor = progractor.getInstance(); // returns the "driver" object
    ```

```
// Step 1: find elements on the page
// * both of these functions throw exceptions if they fail to find something
// * both are synchronous in nature
// * findElement returns a promise so you can chain .then() on it
// * both take a "protractor locator object" as an argument
ptor.findElement(locator); // returns a single element
ptor.findElements(locator); // returns an array of elements


// a locator is created by one of
// * these locator creators all come with webdriverJS
var locator = ptor.By.css('.foo');
var locator = ptor.By.xpath('');
var locator = ptor.By.className('');
var locator = ptor.By.id('');
var locator = ptor.By.linkText('');
var locator = ptor.By.partialLinkText('');
var locator = ptor.By.name('');
var locator = ptor.By.tagName('');

// * protractor adds some angularJS specifc locators by adding a client-side
//   lib that gets run in your browser
var locator = ptor.By.binding('{{foo}}');
var locator = ptor.By.select('');
var locator = ptor.By.selectedOption('');
var locator = ptor.By.input('');
var locator = ptor.By.repeater('cat in pets')
var locator = ptor.By.repeater('cat in pets').row(1).column("{{cat.name

// waits for angular to change the DOM
ptor.waitForAngular()

// Step 2: do stuff with the found element
// Usage: ptor.findElement(locator).{one of these methods}
clear();
click();
getAttribute();
getLocation();
getSize();
getCssValue();
getTagname();
getText(); // gets the **visible** text only
isDisplayed()
isEnabled();
isSelected();
sendKeys(); // simulates typing into it
// there is some way of doing key combinations e.g. ctrl+alt+s stuff too
```

## HTTP APIs

### JSONP

Angular can do JSONP

```javascript
// notice the special 'JSON_CALLBACK' string - angular replaces this with it's
// internal callback function
$http
    .jsonp('http://foo.com?callback=JSON_CALLBACK', {
        params: { name: 'Eoin' }
    })
    .success(function (data) {
        console.log(data);
    });
// * notice that the success function only has one arg available

// The code above would insert the following into the DOM
// <script type="text/javascript" src="http://foo.com?callback=angular.callbacks._k&name=Eoin"></script>

// * The response to that from the server would look something like
angular.callbacks._k({ foo: 'bar' });
// * angular will invoke this function behind the scenes and put the args it
//   got from the server into the 'data' argument of your handler functions
```

#### Limitations of JSONP

- You can only do GET requests (no POST|PUT|PATCH etc.)
- Figuring out what went wrong if it fails is hard because you have no way to
  get the HTTP response status code for the request from the browser.
- Security issues
    - The server is sending you arbitrary JS that you will then automatically
      execute. This is expecially problematic if you are hitting servers you do
      not run

## CORS

- Two types of CORS requests: simple, non-simple
    - simple: GET|POST|HEAD with a limited subset of HTTP headers
    - non-simple: anything else (any other verbs or using any headers outside
      the allowed set)

- IE 8/9 has it's own way of doing CORS so angular does not support it. Ouch.

How does CORS work?

1. The browser sends a request with the OPTIONS verb.
2. The browser checks to see if the server responded correctly, if so
3. The browser sends the main request

The first two steps are called the "CORS handshake"

The server indicates what it will allow the browser to do using headers in its
reponse to an OPTIONS request e.g.

- Allow: (what HTTP verbs are allowed)
- Access-Control-Allow-Methods:
- Access-Control-Allow-Headers:
- Access-Control-Allow-Credentials:
- Access-Control-Allow-Origin:
- Access-Control-Max-Age:

WARNING: CORS requests on IE 8/9 are not supported in Angular

What if you cannot configure JSONP or CORS?

- You can setup a proxy on your own server that will forward requests from
  browsers (obviously not ideal in many respects)
- This will not work for a cordova app so in that situation you are limited to
  JSONP or server-proxying (check this???)

## Promises

- Angular includes the `$q` service which is a **lightweight** promise
  implementation - it does not have all the features that libs like 'Q' have.
    - Is it possible to swap $q for something else that implements the Promises
      spec???
- AngularJS uses promises heavily e.g. in `$timeout` and `$http`

```javascript
var Person = function (name, age) {
    this.name = name;
    this.age = age;
    this.beHungry = function (reason) {
        console.log('Hungry because ' + reason);
    };
};
Person.prototype.eat = function (food) {
    console.log('Eating ' + food);

    scope.$apply; // is the thing that asks Angular to update the DOM based on the models
};
```

A twist in angular is that the promise is resolved or rejected as part of the
`$digest` cycle. In tests you can use `$rootScope.$digest();` to force resolving
or rejecting the promise. Normally `$digest()` is triggered as part of
`$scope.$apply()`

```javascript
// a rejection from doSomething() is handled by retryThing()
// if retryThing() returns a fulfilled promise, otherHappyThing() will never know anything went wrong
ob.doSomething().then(happyThing, retryThing).then(otherHappyThing, failThing);

// a failure in doSomething() sends execution down the "right hand side" (the error chain)
ob.doSomething()
    .then(happyThing, failLogThing)
    .then(otherHappyThing, failCleanupThing);
```

In general, error handlers in promises can do one of two things

1. recover (handle the error)
2. pass the error on

#### $q.reject()

- The equivalent of throwing an exception
- You can create and return a rejected promise from an error handler and it will
  be passed on to the next error handler.
    - this way you can pass errors down the line - this is very powerful

#### $q.all

- Aggregates promises
- it is resolved iff all the promises that make it up are resolved
- returns a single combined promise that can act as a join point
- if any of the promises are rejected, then the all promise is rejected too

```
var combinedPromise $q.al([ob.doThing('foo'), ob.doOtherThing('bar')]);

// handleSuccess is only called if both doThing() and doOtherThing return resolved promises
combinedPromise.then(handleSuccess, handleFail);
```

#### $q.when

- Sometimes you want to combine the results of synchronous operations (returning
  vanilla JS objects) and asynchronous operations (returning promises)
- In this case `$q.when` allows us to wrap JS objects in a promise so that the
  whole thing is easier to work with

```javascript
$.when('x'); // returns a promise that is resolved with the string 'x'
$q.when({ name: 'Eoin' }); // returns a promise that is already resolved (it can never be rejected) with the value of the arg
// obviously it resolves immediately but the promises infrastructure lets you use `.all` etc. on it
```

We can treat promises as model values! but they advise against using it because
promises returned from a function call are **not** rendered automatically

```javascript
// WORKS:
// <h1> Hello {{name}}</h1>
$scope.name = $timeout(function () {
    return 'world';
}, 2000);
// * $timeout returns a promise
// * it is automatically rendered as soon as it is resolved

// DOES NOT WORK:
// <h1> Hello {{getName()}}</h1>
$scope.getName = function () {
    $timeout(function () {
        return 'world';
    }, 2000);
};
// inconsistent!
```

### How $http users promises

$http returns promises resolved with a response object which has 4 properties

- data
- status
- headers
- config

Promises do not normally have a `success()` or `error()` method but the ones
returnd from `$http` do - presumably this is to be compatible with the usual XHR
stuff

### REST and $resource

You have to include `angular-resource.js` and depend on the ngResource module
form your applications module

## Angular boot

Angular boots in 3 possible ways

1. automatic 1:
    - `angular.js` is evaluated before DOMContentLoaded and you have an `ng-app`
      directive
    - it boots on `DOMContentLoaded` and uses the elment that `ng-app` is on as
      the root
2. automatic 2:
    - `angular.js` is evaluated some time after DOMContentLoaded is fired and
      you have an `ng-app` directive
    - it boots whenever the `angular.js` script is evaluated provided
      `document.readyState` is set to `complete` and uses the element that
      `ng-app` is on as the root element.
3. manual:
    - you do not have an `ng-app` directive
    - you have to call `angular.bootstrap(rootElement ['myAppModule'])` manually
    - you should call it in a callback attached to the DOMContentLoaded event
      (not before because Angular needs the DOM)

`BEGIN CHAPTER 11`

what are the ways you can create models in angular how do you link a JS object
into being an angular model that is available in HTML templates

Angular built-in services

$parse

- used to evaluate angular expressions against a scope
- used to set a model's value on a scope

scope.$watch()

- a method that watches model mutations and execute functions in response to
  them. It is called as `scope.$watch(watchExpression, modelChangeCallback)`
  where
- it does model observing in angular - you give it the model (or an expression
  that evaluates to a model) to watch and it will invoke the callback whenever
  the model changes
- it is the method that actually does the changing but it doesn't decide
  **when** the changing should be run

## How Angular decides when to run

What makes angular evaluate all the `$watch` expressions (which checks for model
changs)?

- Angular does NOT use a poling mechanism to periodically check for model
  changes

There are only four kinds of things that could cause Angular to have to
re-render the DOM:

1. DOM events
2. XHR responses firing callbacks
3. Browser location change
4. Timers firing callbacks

Unless one of these has happened nothing can have changed!

AngularJS uses `scope.$apply` to update in-memory JS objects (models). There is
no magic in deciding when it is called - the built-in angular services happend
do it whenever the stuff could possibly have changed the DOM

scope.$apply is _how_ angular updates models. There is no magic about the _when_

- the built in directives and services just have calls at the right time.

* The default directives & services invoke `scope.$apply` whenever a change
  happens. If you write your own, you have to invoke it too.
* calling `$apply` on a scope is what kicks off the model-changes tracking stuff
* Standard services call `$apply` in response to any of the 4 event classes
  above e.g.
    - `$http` calls `$apply` when any XHR callbacks fire - this is why it is
      important to do our XHR stuff through `$http`.
    - `$location` calls `$apply` on the root scope (???) when the location bar
      changes
    - `$timeout` calls it on a scope (not sure which one -prob root scope) when
      timer callbacks are fired. This is why timer stuff should be done through
      $timeout

How does angular update the DOM in response to model changes?

It uses `$digest()`

- a method on all Scope instances
- invoked as part of `$apply`
- it evaluates all the watches registered on all the scopes
- it does the bit of updating the DOM based on changes in the models
    - you might have updated the model in JS or it might have updated based on
      some other DOM change
- it figures out which bits of the DOM need to be repainted
- it postpones DOM repaints until the last possible moment to avoid unnecessary
  repaints
    - the browser is constantly switiching between the rendering thread and the
      javascript execution thread
    - angular runs in the JS execution thread and it doesn't give control back
      to the render thread until the changes for as many models as possible have
      settled
    - this prevents the browser from having to repaint after each model change
        - this results in less context switching and less repaint work

How does `$digest()` know that a model has changed?

It uses dirty checking `$scope.$watch` is what is is executed by the digest
loop. When you register a watch it's value is stored. Basically a bunch of
watches are set in a scope and the digest loop iterates over them and checks to
see if exeting the watch expression this time is different from the stored
value. If it is different, the new value is stored and the watch's callback is
run. Often that callback will directly update the DOM with the new value

Sources of watches

1. we register them manually ourselves
2. any interpolation expression ({{expression}}
3. built-in directives may register their own watches

Angular will keep executing the digest loop and re-evaluating all the watches
until **there are not changes reported**. It keeps going until it gets a
completely clean run! It does this because watch callbacks can have side effects
on other models

You can manually register watches which mutate other models in the scope e.g.
having an end date always be after a start date (it seems like this would be
easy to get into a loop!) so it can take a few runs around the digset loop for
models to settle down

- Angular will kill the digest loop after 10 turns if the models have not
  stablised - it will report an error using the $exceptionHandler service (which
  logs to console by default)
    - Angular will then give over control and the rendering thread will render
      whateve the last value of the model was

Each watch expression is executed at least twice in each turn of the digest
loop. I don't understand this at all :-( This might be a typo in the book?

```
<input ng-model="'name'"> <!-- this is a directive which sets up a watch -->
{{name}} <!-- this is an expression which also sets up a watch -->
```

And each digest loop will have at least one turn, maybe more

As a user you usually don't directly invoke a `$digest` - it happens as part of
an `$apply`

Each turn of the
$digest loop recomputes **all** the watch expressions on
**all** scopes starting from the `$rootScope`

- This seems like extra work but changes in a child scope could influence
  changes in the parent scope
- it traverses the scopes using depth first traversal (TODO: research exactly
  what this is)

## Angular performance boundaries

- Humans cannot process more than 2000 pieces of info on a single page
- Interactions quicker than 50 ms seem instant to humans
- => We have 25 uS per comparision in angular - this is usually fine but you can
  create slow comparisions

### Debugging Angular performance problems

Batarang shows

- the execution time for each individual watch
- the % of total digest time that each watch was

`UP TO START OF 'PERFORMANCE TUNING OF ANGULARJS APPLICATIONS'`
