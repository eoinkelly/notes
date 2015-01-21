# Ember Components (Ember in Action Chapter 7)

Q: what is the right granularity for a component?
    * EIA book advocates for very small components with one job - very SRP

Q: what are the criteria to consider when deciding between a view and a component?
    * will this view be reused in different templates in your app? = medium for component
    * Will this view be reused across apps = strong  for component
    * Does this view require access to a lot of state from the app? = ???
    * Does this view need to exchange messages with other views/components in the app? = ???

Q: is there a performance penalty to compoents vs views?

Q: Is there a performance penalty to using lots of components?

Q: Should _everything_ be injected into a component?
  * Can it know about your route names? EIA seems ok with this - it hard codes route names in components

Q: At what point would you not use a component and use a view instead?

Q: How many injections can you have before component is the wrong thing?

Q: How do you avoid all the passing out of messages I had to do in polytech?

Q: How (if at all) do Ember components handle encapsulating CSS like web components will?

What is a component?
    * part of your app that you can reuse in _multiple_ places _without modification_.
        * multiple places!
        * without modification!

component = template + JS object
* the JS object has two roles
    1. view (it is the target for DOM events from template)
    2. controller (it is target for actions from template)

## Naming

* template name must begin with `components/` and also contain a `-` e.g. `components/my-awesome-thing`
* the dash is designed to prevent a name clash with the final web components spec

A component is both a view and a controller so has

```js
// things you can put in a component

// from the view
// *************

tagName: 'div', // name of the tag to wrap the component in
ariaRole: 'someStringRoleName', // e.g. 'button'
classNames: ['foo', 'bar'], // class names, will be merged with inherited classNames prop (check this???)

// if propertyA is a property its value will be used, otherwise 'propertyA'
// (string) will be the classname - this lets classNameBindings set static classes
// too (but is perhaps confusing)
classNameBindings: [ 'propertyA', 'propertyB' ], // props that return arrays or single class names

attributeBindings: ['propC', 'propD'], // props that return arrays or single attribute (propName: valueName becomes propName="valueName" in HTML)

// these are fine in views but deprecated (in favour of layout and layoutName) in components - why???
template: Ember.handlebars.compile('<h1>{{name]}</h1>'), // a function
templateName: 'psots/new'



defaultTemplate: // function, used if template is null
layout: // function, wraps `template`, contains a single {{yield}}
layoutName: 'string' // name of the template to use as layout

isVisible

context: someController, // set the object that will be used as context for the template
controller: someController // alias for context (I think!!)
// if neither above are set then the parentView's context is used
parentView: someOtherView // ??? in a component
element // current DOM element of the view

// 3 ways to respond to event

// 1. methods named after the events
click: function(event) { },
mouseup: function(event) { },

// 2. event manager

eventManager: {
    // will be called if a click comes from the rendrered HTML of this template/layout or **any of its DOM descendents**
    // this `click` handler will take precedence over any methods named after events in a descendent view
    click: function(event, view) { },
    mouseup: function(event, view) { },
}

// 3. {{action}} helper in template or layout
// properties of the actions object will be invoked if the same named action is used in template
// {{action 'foo'}} // on a click
// {{action 'foo' post}} // send a parameter to the function - in pseudocode is `actions.foo('post')`
{{action 'foo'
        preventDefault=false // turn off jquery preventDefault (which is on by default)
        bubbles=false       // don't bubble up through controller (think this is true by default in component)
        on="doubleClick"    // trigger action on a specific DOM event
        allowedKeys="any"   // allow the action to happen with modifier keys pressed
        target=someView     // send action to a view/controller that is not the default one
        }}

actions: {
    foo: function(???) { }
}

// Note: All properties are accessible from within functions of the view using `this.propName` e.g. `this.controller`

// events
didInsertElement
    * correct place to setup jQuery stuff
    * ??? should jquery stuff be put in afterrender ???
willInsertElement
willDestroyElement
    * correct place to tear down jQuery stuff
willClearRender
    * called when view is about to re-render
    * good place to unhook any listeners that might be on DOM descendents of this
      view for which the DOM elements might be about to disappear
parentViewDidChange (is this the thing that includes the component???)
```


# QUESTION: when I sendAction from a nested component does it go to parent component or controller?

ANSWER:

They go to the controller by default but you can override the `targetObject`
property of the component to be the `parentView` (which is the including
component).

QUESTION: is this a recommended practice?
    -- component is aware of its surroundings (it is no longer totally generalized)

```js

App = Ember.Application.create();

App.ApplicationController = Ember.Controller.extend({
  actions: {
    handleAction: function() {
      alert('Handled in ApplicationController');
    }
  }
});

App.Level1Component = Ember.Component.extend({
  action: 'handleAction',
  actions: {
    handleAction: function() {
      alert('Handled in Level 1');
      this.sendAction();
    }
  }
});

App.Level2Component = Ember.Component.extend({
  action: 'handleAction',
  targetObject: Em.computed.alias('parentView'),
  actions: {
    handleAction: function() {
      alert('Handled in Level 2');
      this.sendAction();
    }
  }
});
```

```hbs
<script type="text/x-handlebars">
      {{#level-1}}
        {{#level-2}}
          <button {{action 'handleAction'}}>
        {{/level-2}}
      {{/level-1}}
  </script>

  <script type="text/x-handlebars" data-template-name="components/level-1">
    <h1>Level 1</h1>
    {{yield}}
  </script>

    <script type="text/x-handlebars" data-template-name="components/level-2">
    <h2>Level 2</h2>
    {{yield}}
  </script>
```

# QUESTION: What is difference between {{yield}} and {{outlet}}

`{{yield}}` has 2 uses

1. in a `layout` it marks where the `template` should be rendered
2. in a component it marks where the block (if any) provided to the component
   should be rendered - remember that nested components are done this way.
