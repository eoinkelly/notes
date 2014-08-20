// Ember structure
// ============
// * template knows about its controller
// * template does now know about its model (the controller proxies properties from the model)
// * the controller knows about the model but not the template
// * from the template perspective a controller is simply an object that provides properties
// * to test a template you just provide an object that will have the properties that the template expects
// * templates and controllers are bound in a 1:1 relationship

// Ember.ObjectController
// ==========
// * a controller that (as well as the usual controller jobs) will proxy an Ember.Object into the template
// * this means you can use {{propName}} in the template rather than {{model.propName}} - this is the main convenience of ObjectController

// Ember.ArrayController
// ==========
// * the main convenience of Ember.ArrayController is that it proxies the array in such a way that you can do {{#each controller}}
// * this transparent proxying means that the template doesn't have to know how the controller is implemented which makes testing templates easier

1.hello

window.App = Ember.Application.create({
  LOG_TRANSITIONS: true,

  ready: function() {
    // this = window.App (the instance of the ember application object)
    // arguments = []

    // How can I call the default Ember implementation of this method?
    // ==============
    // this. _super();
  },

  customEvents: function() {
  },

  // Specify the root elment as a DOM element or jQuery selector string
  // rootElement: '#app, 

  // TODO: other hooks in here?
  //
  // we are calling create here on an ember object - the normal create() rules apply
  // we are overriding ordinary functions, static properties but not computed properties
  // does Ember.Application have computed properties?
});

// How do I control when Ember boots?
// ===========
// * Ember calls calls Ember.Application.initialize() which initializes (boots) the app
// * Methods:
//    * Ember.application.deferReadiness()    // call this after you call Ember.Application.create()
//    * Ember.Application.advanceReadiness()  // call when I am ready for app to initialize

// Router

App.Router.map( function() {
  this.route( 'simple' );
  this.resource( 'posts', function() {
    this.route( 'post' );
  });
});

// Ember Transition
// ================

// * represents a transition attempt
// has methods
// .abort()
// .retry()
// transitionTo() will implicitly abort the current transition
// * gets passed in as a param to various Route hooks
// * the transition object represents the transition
// * it is passed to the various _routes_ that are involved in the transition (source route, destiniation route)
// * the source route can intercept the transition with the willTransistion hook
// * the destination route can intercept the route with beforeModel, model, afterModel


App.ApplicationRoute = Ember.Route.extend({ 

  // TODO: keep callbacks in order that they are called by ember
  
  // 1
  beforeModel: function(transition, queryParams) {
    // this = current instance of App.ApplicationRoute
    // arguments = [Transition, queryParams] 

    // * this is the first hook called when you attempt to transition into a route
    // * use this when you want to redirect without attempting to resolve the model
    // this.transitionTo( 'posts' );
    console.log('beforeModel callback');
  },

  // 2
  model: function( params ) {
    // return this.store.find( 'page' );
    console.log('model callback');
  },

  // 3
  afterModel: function(model) {
    // arguments = [model, Transition]
    // * invalidate this attempt to enter this route
    // * will run the beforeModel, model, afterModel hooks again within the new redirecting transition
    // this.transitionTo( 'posts' );
    console.log('afterModel callback');
  },

  // 4
  redirect: function(model) {
    // this = the instance of App.ApplicationRoute that Ember created for us
    // arguments = [model, Transition]
    // * will NOT run the beforeModel, model, afterModel hooks again within the
    //   new redirecting transition i.e. the model is considered to be validated
    // this.transitionTo( 'posts' );
    console.log('redirect callback');

    // Allows you to override the default controller name
    // * It does NOT allow you to get a reference to the currently active controller
    // this.controllerName = 'foo';

    // * Allows you to override the default template name
    // * It does NOT allow you to get a reference to the currently active template.
    // this.templateName = 'foo';


    // * The route will figure out what template and view to used based on the name of the route
    // * Allows you to override the default view name
    // * it does NOT allow you to get a reference to the currently active view.
    // * Usecase: when you want multiple routes to use the same view
    // this.viewName = 'postList';

    // How do I get a reference to the current controller from a route hook?
    // ==========
    // debugger

    // this.get() supports the unknownProperty handler (sort of a method_missing for properties)
  },


  // ?
  setupController: function (controller, model) {
    // model = the model from the model hook
    // controller = the controller for the current route (resolved based on the name of the route)

    // default action of this function:
    // controller.set('model', model)

    // If you override this function and still want this behaviour to happen you need to do it!
  },

  actions: {
    someAction: function() {
    }
  },

  // TODO find out more aobut his hook
  didTransition: function () {

    Ember.run.once(this, function () {
      // this.get('router') // => the router singleton
      // this.get('router').get('url') === this.get('router.url')
      trackAnalytics(this.get('router.url'));
    });
  }

});


App.IntroPageRoute = Ember.Route.extend({
  
  page: 0,
  
  model: function( params ) {
    // Figure out the page number
    this.set( 'page', parseInt( params.number, 10 ) )
    // Grab model from parent model (collection)
    return this.modelFor( 'intro' )
      .objectAt( this.get( 'page' ) )
  },
  
  afterModel: function() {
    
    var currentPage = this.get( 'page' )
    var pageCount = this.pageCount()
    
    currentPage === ( pageCount - 1 ) ?
      this.controllerFor( 'intro' ).set( 'isLastPage', true ) :
      this.controllerFor( 'intro' ).set( 'isLastPage', false )
    
    currentPage === 0 ?
      this.controllerFor( 'intro' ).set( 'isFirstPage', true ) :
      this.controllerFor( 'intro' ).set( 'isFirstPage', false )
    
  },
  
  actions: {
    
    previous: function() {
      
      var currentPage = this.get( 'page' )
      var pageCount = this.pageCount()
      
      var nextPage = Math.max( 0, currentPage - 1 )
      
      this.transitionTo( 'intro.page', nextPage )
      
    },
    
    next: function() {
      
      var currentPage = this.get( 'page' )
      var pageCount = this.pageCount()
      
      var nextPage = Math.min(
        currentPage + 1, pageCount - 1
      )
      
      this.transitionTo( 'intro.page', nextPage )
      
    }
    
  },
  
  pageCount: function() {
    return this.modelFor( 'intro' ).get( 'length' )
  },
  
  renderTemplate: function( controller, model ) {
    var template = App.layouts[ model.get( 'layout' ) ]
    this.render( template )
  }
  
});

App.DemoRoute = Ember.Route.extend({
  
  model: function() {
    return this.store.find( 'page', 1 )
  },
  
  renderTemplate: function() {
    var key = this.currentModel.get('layout')
    this.render( App.layouts[key] )
  }
  
});

App.SimpleRoute = Ember.Route.extend({
  
  model: function() {
    return this.store.find( 'page', 1 )
  },
  
  // renderTemplate: function() {
    // args?
  // }
  
});

// * the purpose of an ember controller is to decorate it's model with display logic
// * "is this logic solely related to how the model displays on screen?" if so, then it belongs in the controller.
// * the controller acts as a proxy for the model
App.SimpleController = Ember.ObjectController.extend({

  simpleStaticProperty: 12,

  simpleComputedProperty: (function() {

    // From within a controller computed property ...

    // what is 'this'?
    // ===============
    // this = the controller

    // what are args?
    // =============
    // arguments = ['simpleComputedProperty']
    // * it seems to be an array with just the property name in it ???

    // how do I get at my model
    // ========================
    // this.get('model') // works - if it is failing it might be because  you have overriden setupController but not assigned the 'model' property
    // * this.model does not work because ???
    //    ? model is a computed property of the controller object
    // * if I am extended from Ember.Controller I can still get at my model 
    //   but the template will not be able to access model properties

    // How do I get an attribute of my model?
    // ======================================
    // this.get('model').get('someAttr')

    // how do i get at my route object?
    // ============================
    // ???


    // how do i get a reference to the application-wide router?
    // ========================================================
    // this.get('target') 
    // * gets the object to which actions from the view should be 
    //   sent (by default this is the router)

    // How can I trigger an action as if it came from my template?
    // ========================================================
    // * send an action to the **router** which will delegate it to the currently 
    //   active route heirarchy as per the usual action bubbling rules
    // * this mimics what would happen if a template triggered an action
    // this.get('target').send('action-name')

    // how do I get a ref to a different controller?
    // ============================================

    // option 1:
    // ---------
    // this.controllerFor('other-route-name') // works but is depreacted in favour of this.needs

    // option 2:
    // ---------
    // * you can specify the controllers you want to access in the `needs` array
    // * any controllers you add to needs will be accessible through the 'controllers' property
    // this.needs = ['post'] // an array of route names (ember can work out the corresponding controller names)
    // postTitle: (function() {
    //   var currentPost = this.get('controllers.post');
    //   // TODO: it seems the arg to get() can be more than just a simple identifier ???
    //   return currentPost.get('title');
    // }).property('controllers.post.title')
    // TODO: figure this out

    // how do I get a ref to another controller's model?
    // =================================================
    // ???

    // how do I get a ref to another route in the system?
    // ==================================================
    // ???


    return "hello";
  }).property(),

  actions: {

    // How do invoke an action from another action?
    // ============================================
    // TODO: waht is the context ???
    doThing: function() {
    },
    doOtherThing: function() {
      this.send('doThing', 'some.context');
    }
  }

});


// How do I set computed properties on an object when I create it?
// ============


// Simple object factories
// =======================

// Ember .extend creates a factory
App.SomeObject = Ember.Object.extend({

  init: function() {
    console.log('init arguments', arguments);
    // this = the object being created
    // arguments = []
    // * init does not get the args you pass to create()
    // * ? is init called before/after the create properties are set?
    // Tip: always call super if you override an Ember framewrok class
    // this._super();
  },

  simpleProp: 'hello', // default value

  computedProp: (function() {
    return "complex";
  }).property(),

  simpleFunc: function(a, b) {
    // this = the current instance of App.SomeObject 
    // arguments = whatever you passed in manually

    this.get('simpleProp');   // works
    this.get('complexProp');  // works

    return "some stuff";
  }

});

App.ChildObject = App.SomeObject.extend({

  init: function() {
    console.log('init arguments', arguments);
  },

  // default value
  // * since you can add properties at any time, you only need to set
  //   properties here if you want to set defaults for all instances of them.
  childSimpleProp: 'default value' 

});

// You can re-open both ember ojects and ember classes to add more stuff to them
// * reopenClass() 
// * adds static properties to the class object itself - they are only
//   available on the class object - they are not available on instances
//
App.SomeObject.reopenClass({
  extraStuff: function() {
    console.log('extra stuff');
  }
});

// If you re-open an instance, it adds stuff to the instance's prototype which
// makes that new stuff available to all instances
// thing.reopen({
//  newStuff: function() {
//    console.log('do new stuff');
//  }

// });
// Question: how are the results of reopen() and reopenClass different?
// does extend put the stuff you give it into the prototype?


// Create instances
// ================

var thing = App.SomeObject.create({

  // NB: only set simple properties in the hash you pass to create!
  // * if you try to pass a computed property it will get overwritten by the prop you pass in
  simpleProp: 'eoin',

  // passing in computed properties to create() is **not** allowed for performance reasonsjkkjkkk 
  // * why ???
  // computedProp: 'other' 

});

var childThing = App.ChildObject.create({
  childSimpleProp: 'unique value'
});

thing.get('simpleProp');    // works
thing.simpleProp;           // works
thing.get('computedProp');  // works
thing.computedProp;         // returns undefined - does NOT work 
thing.simpleFunc(23,44);    // works

// it seems you can't get the properties of parent object
// so what does inheritance get you?
childThing.get('simpleProp'); // undefined

// childThing.simpleFunc(); // does not work!

// it seems you can add properties to an object at any time
childThing.set('foofoo', 'bar'); // works

// thing.destroy(); // schedules the object for deletion by the runloop
//
// iterates over each computed property of the object, passing it's name and
// value to the callback
// * why would you want to do this?
// * can you do it for static properties?
// thing.eachComputedProperty(
//

// Experiment: Inheritance in Ember
// ********************************

App.Foo = Ember.Object.extend({
  name: 'Eoin',

  status: (function () {
    return "happy";
  }).property(),


  sayHello: function () {
    console.log('hello from parent');
  }


});

App.Bar = App.Foo.extend({
  age: 34,

  headWear: (function () {
    return "beanie";
  }).property(),

  sayHello: function () {
    this._super(); // call our parent's sayHello()
    console.log('hello from child');
  },

  unknownProperty: function() {
    console.log('unknownProperty called');

    // If this returns anything except undefined, that value will be used as the value of the property
    return "surprise!";
  }

});

var f = App.Foo.create({});
var b = App.Bar.create({}); 

b.sayHello();

b.getWithDefault('name', 'defaultName');

// Decoding Ember's toString()
// **************************

f.toString(); // "<App.Foo:ember210>"
b.toString(); // "<App.Bar:ember211>"
// debugger;

// the first pit is the class that it extends from and the bit after the colon is a unique identifier
// what's in an instance of an ember object? 
// seems to have a ref to some sort of meta object with keys for
// * cache, descs, proto, source, values, watching
// not much else in an object
// __proto__ points at the object you called create on
// the "emberXXX" thing seems to be some sort of unique ID
// there are unique keys that are diff every time you run ember (but are the same for all objects in the same instance of the app)
// __ember1394179927031: "ember202"
// __ember1394179927031_meta: Object
// __ember1394182320643: "ember210"
// __ember1394182320643_meta: Object
//
// concatenated properties
// when you implement a property in a child class and a parent class, it usually just overrides it. There are times when it would be handy to have the child property and the parent one concatenated together - ember supports this with concatenatedProperty
// why not just have the child property call this._super() ?
//    * ember can do it for arrays of stuff - even for things that are not
//      computed properties => you can use a static property for something that
//      would normally be computed


// Views
// *****

App.AudioPlayerComponent = Ember.Component.extend({

  // How do I create a static property for my templates?
  // =============

  // access in template as {{simple}} 
  simple: "i am simple",

  // How do I create a computed property for my templates?
  // =============

  // access in template as {thing}} 
  thing: (function() {
    // this = current instance of the component
    // arguments = ['thing']

    return "hello";
  }).property(),

  // How do I access a parameter passed to the component?
  // this.get('paramName');

  actions: {
    doSomething: function() {
      // this = the component instance
      // arguments = ?
    }
  }

});



// Implicit mixin passed to extend()
var Foo = Ember.Object.extend({
  greet: function () {
    console.log('hello');
  }
});

// Explicit mixin
var Mixy = Ember.Mixin.create({
  greet: function () {
    console.log('hello from mixin');
  }
});

// Passing explicit mixin
var Bar = Ember.Object.extend(Mixy);


var b = new Bar();
var f = new Foo();

console.log(b.greet());
console.log(f.greet());

// ***********************
// create = extend + instantiate
var Constructor = Ember.Object.extend({
  name: 'Eoin',
  age: 34,
  init: function (a) {
    console.log('i am init()');
    console.log('I got:' + a);
  },
  
  doOther: function () {
    console.log('i am also run on init');
  }.on('init'),
  
  doThing: function () {
    console.log('hellohello');
  }.on('hello'),
                                      
  doOtherThing: function () {
    console.log('oh hai hai');
  }.on('hello')                                    
});

var a = new Constructor('i am arg');

// create()
// ********
// + lets you extend() and instatiate at the same time
// - you cannot pass args to the constructor
// - the code that creates the object knows a lot more 
//   about it's insides (compared to new Foo())
var b = Ember.Object.create({
  name: 'Eoin',
  age: 34
});

console.log(b.name);
console.log(b.age);

// Ember as event dispatcher
// *******************

// * Any ember object can be sent events
// * It can have multiple functions that respond to them
Ember.sendEvent(a, 'hello');

console.log('***************************');
console.log('***************************');

// Ember observers
// ***************

var Thing = Ember.Object.extend({
  name: 'Eoin',
  title: function () {
    console.log('Mr. ' + this.get('name'));
  }.observes('name')
});

var t1 = new Thing();
console.log(t1.get('name'));
t1.set('name', 'Eoin Kelly');

var Thing2 = Ember.Object.extend({
  name: 'Colm'
});

// notice that we are adding the observer to a single instance of Thing2 
// rather than all instances of Thing (which the other syntax did for us)
var t2 = new Thing2();
Ember.addObserver(t2, 'name', function () {
  console.log('Sir. ' + this.get('name'));
});

console.log(t2.get('name'));
t2.set('name', 'Colm Kelly');
