
## Passing things to ember components

* ember components are isolated by default
* they use a DI system where you inject everything the component needs from the app
* the handlebars syntax defines property **binding** - it is not like passing a copy of a value to a function
    * the bindings are live and two-way by default
    * except for literal values obviously
* you can create bindings for a component in two ways
    1. positional bindings
        * -- is more fiddly within the component than named bindings
        * ++ it is handy if you want to pass an arbitrary number of args
        * you have to create a static `positionalParams` array in the component which lists the names the bindings should have within the component
        * you can set `positionalParams` to a string which will cause the component to accept any number of positional params and put them all in the property that matches the string
    1. named bindings i.e. `key=value` syntax

* we will describe this syntax as `lhs=rhs`
* the lhs is always the name that this value will take on within the child component
* the rhs can be
    1. a literal primitive value
    1. a reference to a function
    1. a reference to a property available in the calling template

#### nested helpers in a template

Helpers have 3 forms of invocation

1. inline
2. block
3. nested
    * nested: surround the nested helper in () e.g `{{if isFast (if isFueled "zoooom")}}`


### helpers: {{hash}}

* nested helpers surrounded by () will be evaluated
* Use the {{hash}} helper to create a hash to pass as an option to your components. This is specially useful for contextual components where you can just yield a hash

`{{yield (hash body=(component editStyle))}}`
```
u = {{component editStyle}}
v = {{hash body=u}}
{{yield v}}
```

### where to put event handlers

Terminology: things defined inside the `actions` property are called "closure actions"

* event handlers defined direclty on the component must have the name of the 28 or so supported browser events. They also get the jquery event object as a param
* closure actions can be defined inside `actions` property of component
    * closure actions ONLY get the browser event action if they are called as **inline** handlers

```
// inline handler (browser event object is passed):
<button onclick={{action 'doThing'}}>Do thing</button>

// non inline handler (browser event object is not passed)
<button {{action 'doThing'}}>Do thing</button>
```

#### helpers: {{action}}

http://emberjs.com/api/classes/Ember.Templates.helpers.html#method_action

> The {{action}} helper provides a way to pass triggers for behavior (usually just a function) between components,
> and into components from controllers

* any function can be passed to the `{{action}}` helper, including other actions
* `{{action}}` always returns a JS function

There are three contexts an action helper can be used in.

1. attribute context
2. handlebars value context
3. element space

```
{{! An example of attribute context }}
<div onclick={{action "save"}}></div>

{{! Examples of Handlebars value context }}
{{input on-input=(action "save")}}
{{yield (action "refreshData") andAnotherParam}}

{{! An example of element space }}
<div {{action "save"}}></div>
```

in _attribute context_ and _handlebars value context_:

In these contexts, the helper is called a "closure action" helper. Its behavior is simple: If passed a function name, read that function off the actions property of the current context. Once that function is read (or if a function was passed), create a closure over that function and any arguments. The resulting value of an action helper used this way is simply a function.

* you can do partial application with {{action}} helpers! it will

```
{{action 'foo' 11 'bar'}}
```

1. find the property `actions.foo` in the current context (which will be a function)
1. apply that function using the current context and curry the args given in the handlebars (`11` and `'bar'` in this case)
    * => within an action handler, you can rely on `this` pointing at the same context as the template it was invoked in has
    * => you can add more args when you actually invoke the action and they will be curried on to the end of the argument list
    * => you can pass around the result of `{{action}}` to another component and it will still execute in the context of the template it was defined in
        * => you can pass the action-function to a child component for it to execute without it executing in the child function's context

TODO: test this

in _element space context_:

* the {{action}} helper provides a useful shortcut for registering an HTML element in a template for a single DOM event and forwarding that interaction to the template's context (controller or component).
* If the context of a template is a controller, actions used this way will bubble to routes when the controller does not implement the specified action.
* Once an action hits a route, it will bubble through the route hierarchy.

* this is the only context where actions "bubble" up to the controller and routes

#### Positional params

```
{{my-thing foo bar}}

import Ember from 'ember';


```

```
// within a component
this.sendAction('foo') // TODO: WHAT DOES THIS DO?


{{my-comp foo=(action "blah")}}
{{my-comp foo=(action "actionName" "arg1" target=objectTarget)}}
// the default target is the component itself - you can change the target to be a *property of the component* e.g. for injected services

// value allows the parent component to destructure the value it gets out of hte child component (saves you writing some boilerplate)
{{my-comp foo=(action "actionName" "arg1" value="foo.blah")}}
```



```
// these are the same
this.sendAction() // if not action name is passed, the action name defaults to 'action'
this.sendAction('action')

{{my-comp action=(action "blah")}}
```
