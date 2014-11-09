/*
Goals
  be usable as an editor snippet for people new to Ember
  to be a concise documentation of the router

Includes
  shows the default implementation of each hook
  shows what params each hook gets
  shows how to get to other things from each hook (and when you can't shouldn't
  shows value of this in the hook

header
  link to my future blog post about how ember routing happens

todo
  keep callbacks in order that they are called by ember
  make this use es6 export
*/


App.{foo}Route = Ember.Route.extend({

  /**
   * A Route object has X sections
   *
   * 1. Configuration
   * 2. Hooks
   * 3. Action handlers
   *
   *
   * Entering this route:
   *
   * 1. beforeModel
   * 2. model (unless a model is provided)
   * 3. afterModel
   * 4. redirect
   * 5. activate
   * 6. setupController
   * 7. renderTemplate
   *
   * Leaving this route
   *
   * 1. deactivate
   *
   */

  // name of controller to load from the container. defaults to same name as route
  // controllerName: '{foo}',

  // query params configuration hash
  queryParams: {
    category: {
      refreshModel: true // opt in to full transistions (I think)
        replace: true
    }
  },

  // 1
  /**
   * beforeModel
   *
   * this is the first hook called when you attempt to transition into a route
   * use this when you want to redirect without attempting to resolve the model
   *
   * @param transition
   * @param queryParams
   * @this current instance of this route
   *
   * @return nothing by default but if promise then ember will wait
   *
   */

  beforeModel: function(/* transition, queryParams */) {

    // Common tasks

    // Abandon the current transition and go to a different route
    // this.transitionTo('other-route-name');
  },

  // 2
  /**
   * model
   *
   * If you return a promise from this function Ember will wait for it to
   * resolve before continuing the transition.
   *
   * @return Object that will be used as model
  model: function(/* params */) {

    // Default behaviour
    // return this.store.find('foo');
  },

  // 3
  /**
   * afterModel
   *
   * @return nothing by default but if promise then ember will wait
   *
   */

  afterModel: function(/* model, transition */) {
    // arguments = [model, Transition]
    // * invalidate this attempt to enter this route
    // * will run the beforeModel, model, afterModel hooks again within the new redirecting transition
    // this.transitionTo( 'posts' );
    console.log('afterModel callback');
    // (In fact, the default implementation of afterModel just calls redirect.)
  },

  // 4
  redirect: function(/* model, transition */) {
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

  activate: function () {
  },

  // ?
  /**
   * setupController
   *
   * @param controller The instance of a controller object that Ember found in
   *                   the container based on the name of this route.
   * @param model Return value of this route's model hook
   *
   * @this ???
   *
   * @return nothing
   *
   */

  setupController: function (controller, model) {

    // default action of this function:
    // If you override this function and still want this behaviour to happen you need to do it!
    controller.set('model', model)

    // Common tasks

    // Get access to another controller in the app
    // this.controllerFor('topPost')
  },

  /**
   * renderTemplate
   *
   * Implement this to render a template other than the one associated with the route handler
   *
   */

  renderTemplate: function(/*???*/) {
    // Default behaviour
    this.render('{foo}'); // TODO: check this
  },

  /**
   * resetController
   *
   * set query param values back to their defaults before exiting the route or changing the route's model
   *
   * TODO: where is this called
   *
   */

   resetController: function (controller, isExiting, transition) {
    if (isExiting) {
      // isExiting would be false if only the route's model was changing
      controller.set('page', 1);
    }
  }
  /**
   * Handlers for actions that have bubbled up from the controller.
   *
   */

  actions: {
    // what params do actions get?
    someAction: function(/* ??? */) {
      // is return value of action used?
    }
  },


});

