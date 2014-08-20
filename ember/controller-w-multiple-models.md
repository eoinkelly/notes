# Question: My controller/template needs access to multiple models. How do I do this?

## Alternatives

1. Load one model and use its associations to get the others you need
    * You do nothing special in the route and let the controller/template do all the work
    * pros
        * easy to do I guess
    * cons
        * things get arkward if **any** of the associations are async as the controller has to deal
          with them since template cannot handle promises.

2. Return an RSVP.Hash from the model hook with the values being the models you need
    * your model property in the controller is a hash now
      * => you have to namespace each lookup by key which means that no one key is seen as more important
          * there are times when this is exactly what you want!
    * pros
        * it is a pretty neat way to do it provided you don't have any dynamic segments in the route.
    * cons
        * the model hook is skipped if you enter the route from a {{link-to}} helper that provides a model.
          This means you would get different behaviour depending on how the route was entered - BAD!

3. Set the other models as properties on the controller in route#setupController
  * this makes whatever route#model returns the "primary" model
    * this means that {{bar}} in a template will lookup this model
  * your templates will have to ask for the other stuff by name

## General principles

* If you find yourself dealing with a promise in the controller layer you are doing it wrong.
* The #model hook is best tuned for returning one model because it is skipped by {{link-to}} helpers
    * The setupController hook seems like the best place to add extra models

```js
setupController: function (controller, model) {
  controller.set('model', model); // default behaviour
  this._super(controller, model); // safer way of doing default behaviour
  // model has been resolved by the time this is invoked so the controller gets the settled value


  controller.set('other', this.store.find('other', 12))
  // ??? does the controller get a promise or the value in this case ???

  // do I need to do ... ???
  controller.set('other', {}) // some default value if undefined is not ok
  this.store.find('other', 12).then(function (other) {
    controller.set('other', other);
  })

  // Have I setup an observign relationship now?

  // the default setupController does not return a value so return values are ignored
}
```
