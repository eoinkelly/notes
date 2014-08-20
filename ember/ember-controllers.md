# Ember Controllers

Responsibilities of a controller:

* manage transient state for the app
* decorate models for presentation

Decouple application state from the visual representation of the state e.g.
audio muted. There might be 3 ways that the app can show that the audio is muted
to the user but we only want to store the state once

* The controllers are the source of truth for transient state
* Views can read that state and change what they show in response
* Controllers store the state and the views display it - those are separate jobs!

Views in Ember are temporary - they are destroyed when they are no longer
visible Controllers are singletons and are long lived.

Q: Can you have a controller that has nothing to do with a particular route?
A: Yes. Controller and templates live as pairs e.g. the `foo` template and
  `App.FooController` You can render `foo` and have it backed by the controller in
  any other template with `{{render 'foo'}}`.

What kind of different relationships do needs and templates create?

* Controllers can share state with other controllers via `needs`.
* They can have "parent child" relationships between themselves
    * if A `needs` B the A is the parent of B ???

    * If template for ControllerA includes a view|render for template B then the
      ControllerB will have access to ControllerA via `parentController`

* Controllers form a network of things that manage state.


## Ember.ObjectProxy

* `model` and `content` are aliased to each other in `Ember.ObjectController` and `Ember.ArrayController`
  (but not `Ember.Controller`)

* This means that any property you ask for on the controller will be looked up
  on the `model` (same as `content`) if it is not found in the controller.

## Ember.ArrayController

* `Ember.ArrayController` is based on `Ember.ArrayProxy`
* It decorates both the array itself and the items in the array
* templates for an array controller have 2 scopes
  1. the outer scope is the instance of ArrayController itself
  2. within an `{{#each}}` block, the scope becomes a single item in the array

Templates are always backed by controllers - the router will search for a
controller with a name that matches the template so templates always have a
controller to talk to (whether there is a model behind it is a concern of hte
controller not the template).

## How is `{{render}}` different from `{{outlet}}`?

### {{outlet}} and {{render}}

* both devote a portion of the page to something

### {{outlet}}

* the router decides what should be rendered into it
* you can have multiple outlets on a page but they must be named e.g.
  ```
  {{outlet foo}}
  {{outlet bar}}
  ```

### {{render}}

* `{{render 'template-name'}}`
    * gets the _singleton_ instance of App.TemplateNameController
* `{{render 'template-name' model}}`
    * the template will get a _unique_ instance of the named controller!!! NB!!!
    * the controller will be the proxy for the model (as normal)
* render does not require the existance of a matching route

You cannot have multiple render calls without different models on the same page
```
// This will not work:
{{render 'foofoo'}}
{{render 'foofoo'}}

// This is good
{{render 'foofoo' alpha}}
{{render 'foofoo' beta}}
{{render 'foofoo' gaga}}
```


* each e.g. `{{#each}}`
* {{partial}}
    * takes the name of a template and renders it but it does **not** change the context (unlike render, each)
* `{{view App.SomeView}}`
    * similar to {{partial}} but it renders an `Ember.View` not a template
    * an instance of the provided view constructor will be created
    * the template associated with the view will be used e.g. `App.SomeView` will use `some`

* render|view|partial are "controller creating helpers"
* A template can display data from multiple controllers!
* Template helpers (render, view, partial) tie controllers together into a tree

## {{#each}}
* forces the creation of a new view
* this view will choose a new context for its template
* can accept an argument that says what controller to use e.g. `{{#each thing itemController="foo"}}`
  will try to use `App.FooController` as the context.

a child controller is one that is invoked by {{each}}, `{{render}}` etc.
the parent controller is the one that backs the outer template

Actions bubble through child and parent controllers before they get to the routes !!!

A child controller can access it's parent with `parentController` property


How do I
* ... get a controller from another controller?
  A: use `needs`
* ... get a controller from a route?
  A: use `this.controllerFor()`


Alternatives to wiring controllers together
1. Inject into them via `register()` and `inject()`
2. Use `controllerFor()` in the route

QUESTION: When is it bad to wire controllers together?
QUESTION: are controllers and services the same thing in Ember?

You can think of components as view + controller


Actions will always hit the leaf route but the templates decide what controllers get to see it

How do I send actions to a component?

```handlebars
<div>
  {{my-great-thing body=foo viewName="preview"}}
</div>

<button {[action 'doshit' target=view.preview]}>DO IT</button>
```
