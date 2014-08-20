# Container

The router and templates are fairly unique in ember (as opposed to models, views, controllers etc.)

* ember has exactly one router
* it has access to a store
* it is always an instance
* it maps routes to states <— NB
* it is available on **all** route objects as the `router` property e.g this.get(‘router.url’) in any Route object will get the url form the global singleton router

What properties are in the router object that are useful:

* methods
    * url = returns the current url
* properties
    * location = the type of URLs that the app will use: hash|history|none

templates are just functions - they are not objects - you cannot instantiate them - they take a context object and return a string representation of it

What makes things similar?
shared interface
e.g. all views share the same interface (expose the same set of stuff)
this is often extracted into a shared superclass
shared dependencies
all route objects have access to the router singleton. views do not. => routes have a shared dependency that views do not
shared usage

Thing			| Instantiated 	| New each time? 	| Type	|
 —————————————————————————————
Component		|  Y			| Yes					| Class	|
Controller		| Yes			| No					| Class	|
Template			| No			| No					| Function	|


Shared discovery
how do you find the thing when you need it e.g.
Tempalates = Ember[‘TEMPLATES’]
Models = App.Foo
Controller = App.FooController

Containers in general
---------
containers contain a registry keyed on ‘type:name’ string that can hold constructors.
when you lookup a constructor, the registry gives you back an instance of the object.
the first time you call it it will create the instance, every time after that it just returns the same instance
containers allow objects inside them to ask them to find other objects for them
it functions as a DI system
contaiers can be configured from outside to resolve property requests from objects inside in a certain way e.g. a logger
you can tell containers not to instantiate the object when it finds it e.g. if you store a function or primitive value in there
you can tell the container that a particular key should not be memoized
containers have a resolve function that decides how the param you pass in to lookup should be converted into the acutal key name e.g.
‘IndexController’ -> ‘controller:index’

The resolver is plugged into the container and it does all the finding of things based on their key names

The Ember Application Container
----------------
Ember apps have exactly one container
=> ember objcts have a `container` property that will give you a reference to it: this.container (note you don’t use get())
the keys are colon separated e.g.
router:main

App.register(‘customnamespace:thing’, App.SomeObject) // manually add stuff to the container
App.inject(‘controller’, ‘analytics’, ‘customnamespace:thing’) // control what object will be found when objects in the container do a lookup

You typically run the above methos as part of initializers
needs in controller is a form of DI
needs ‘foo’, // ember does a lookup in the controllers section of the application container

Ember makes App.__container__ available but don’t use it!
