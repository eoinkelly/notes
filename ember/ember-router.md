
## Query Params

* sometimes you have a lot of state in the app that doesn't belong in models
* you want query params to be optional - currently requires 2 routes
* some of that state is controller state - doesn't belong in models

Q: can you achieve same thing as query params with controllers alone?
  mabye a controller that stores the relevant state and all others can get at it
  or a service

Ember treats query params as a routing concern
* query params are global!
* ember has more than one route active at a time => the query params will all be mashed together

```javascript

App.Router.map(function () {
  // PostsRoute will not have access to sort, showDetails
  this.resource('posts', {queryParams: ['sort', 'direction']}, function () {
    // The child will have access to child sort, parent direction, child showDetails
    this.route('comments', {queryParams: ['showDetails', 'sort']} );
  });
})

// queryParams are made available to all the route hooks

App.PostsRoute = Ember.Route.extend({
  // These are pretty good naming
  beforeModel: function (queryParams, transition) {}
  model: function (params, queryParams, transition) {}
  afterModel: function (resolvedModel, queryParams, transition) {}
  setupController: function (controller, context, queryParams) {}
  renderTemplate: function (controller, context, queryParams) {}
});


// just pass an object with a queryParams key
this.transitionTo('posts', {queryParams: { sort: 'yes', direction: 'asc'}});
this.transistionTo('posts/1?sort=yes&direction=asc'); // this also works

{{#link-to 'posts' direction=asc}}go to posts{{/link-to}}

// this one will set direction to whatever 'otherDirection' property of controller is
{{#link-to 'posts' directionBinding=otherDirection}}go to posts{{/link-to}}
```

Note that `context`  is used rather than model - this is because it is not necessairly the model (although it is in the default case)

### Gotchas

* query params are sticky - when they are set on a route, they will remain set unless you explicitly unset them e.g if you transition into `/posts/1?sort=name` and then the template has a `{{link-to 'posts' direction=desc}} ...` then the resulting route will be `/posts/1?sort=name&direction=desc`
    * most of the time this is what you want
* to clear a query param pass in a falsy value
  `{{#link-to 'posts', sort=false}}`
* boolean query params do not have a value in the query string
  `{queryParams: { isOk: true }}` becomes `/blah?isOk
* currently the 3 model hooks are all called again then the query params are changed but plans to fix this

# Obeservers gotcha

Their contents should be wrapped in `Ember.run.once`

```javascript
fooWatcher: function () {

  // Without the run loop,
  // Observers are synchronous so they might be called multiple times in a single run loop
  // which means they might be called with unexpected values
  Ember.run.once(this, function () {

  });
}.observes('thing'),
```
# Presentation: Route traversal with flows

* http://confreaks.com/videos/3312-emberconf2014-controlling-route-traversal-with-flows
* Consider: The URL as the lowest level of abstraction that a normal user will use to interface with the site

3 categories of Routes

1. Resources
    * available at any point in time in the app
    * you want them in the history stack
    * generally they don't change the state of your app
    * use plural nouns for collections, singluar nouns for individual resource
2. Actions
    * Use verbs
    * receives all of hte users input in one hit
    * in response they will see the new resource/flash message/etc.
    * oftne want to completely reset the controller e.g. discard the form stuff they just filled in
    * but JS apps don't have to discard all the state when the user changes route (as you do on the server)
3. Flows
    * a series of actions divided across routes
    * change the application from one state to another across multiple routes e.g. use goes through 3 routes to get from one state to another
        * e.g. login -> authenticate -> accounts
    * your flow has its own state machine
    * your flow is a directed graph with an attached state machine
        * it is not acyclic because the user can go backwards with their back button
        * routes are nodes in the graph

## How do we model flows?
We make a state machine that models how the use moves through the application

  * so that users can jump back to various points

step 1 inventory your routes
    * open the router map and keep it visible
step 2 list linear paths
  * he uses a spreadsheet
step 3 convert paths into node graph

step 4: describe edge traversal

step 5: identify backwards traversals
    * this is usually users hitting hte back button


TIP: replaceWith() is an alternative to transistionTo() execept it doesn't add anything to browser history

Most of the logic of traversing between routes happens in action handlers

* Define all progression in a separate location
* Load the Flow and its state from a route-global injection
* Delegate the identification of where to go to the Flow itself
* Call back into the Flow to progress
* Your flow tracks which states you've traversed in case you need that information in the applicaiton

```javascript
// design an directed graph
// can be edge-list or adjancey-index
// we are using edge-list
LoginFlow.addEdge({
  from: 'login.index',
  to: 'accounts.index',
  weight: 1,
  conditions: ['isIdentified', 'isAuthenticated']
});

var login = Ember.Object.extend();

Ember.Appication.initializer({
  name: 'login',
  initialize: function (container, app) {
    app.register('flow:login', login);
    app.inject('route', 'login', 'flow:login');
  }
});

// ...

beforeModel: function () {
  // looks up the current flow
  // identifies where the user should be
  this.get('flow').check();
}


// ...


actions: {
  authenticate: function () {
    var Flow = this.get('flow');

    ic.ajax.raw('/authenticate').then(function (result) {
      if (result.response.isAuthenticated) {
        // the action *sets* state on the flow but it doesn't do any processing
        Flow.set('isAuthenticated', true);
        Flow.progress();
      }
    })
  }
}

```
