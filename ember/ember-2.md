They like the _data flow model_ from React \* well written ember apps already
use

- 2 will have a virtual DOM
- React virtual DOM allowed them to simplify the programming model of _component
  based applications_.

when you enter a route:

1. it builds a controller
2. associates a model with it
3. hands the controller to the view for rendering

currently ObjectController and ArrayController magically proxy model
properties - this will stop - you will need to use `model.propName`

top level components will **not** perisist across naivation => persistent state
should be stored in route objects and passed into the "routeable component" when
it is instantiated route objects will have an `attrs` hook which can return
other asy,c data that the component might need

currently ember has routable controllers which are persistent, in future it will
have routeable components which do not persist

routeable componetns should be stored on filesystem in "pod" format

```js
// This is a good alternative to using a proxying ObjectController
export default Ember.Controller.extend({
    // Note: alias() is a shorthand for creating a computed property
    firstName: alias('model.firstName'),
    lastName: alias('model.lastName'),

    fullName: function () {
        return this.get('firstName') + this.get('lastName');
    }.property('firstname', 'lastName')
});
```

# Timelines for changes

Ember.ObjectController gone in 2.0 deprecating in 1.11

Virtual DOM stuff defn. in 2.0

One way binding by default - when ???

Routable components - when??

1.9 handlebase 2.0 only has streams has activated and deactivate _events_ (not
just hooks) for Routes has pauseTest() helper has new keyup, keydown actions
deprecated context switching in templates ie. using #each or #with without
`{{#with foo as person}}` or `{{#each foo as person}}` 1.10 has HTMLbars - a new
rendering pipeline made #each and #with block params consistent
`{{#each persons as |person|}}` you can use this with components ember.js file
will become ember.debug.js for clarity the 'childViews' property of a view is
deprecated - use `this.pushObject(this.createChildView(...))` instead
`beforeObserve` feature is deprecated as it slows the oberserver system outlet
names must be quoted e.g. {{outlet "foo"}} not {{outlet foo}}

Aside: compontents can be block helpers and take block parasm as well as named
arguments components are a lot like a ruby function in that respect

Aside: in helpers unquoted words are values and quoted words are strings

# Can components replace partials?

partial does not shift context components in 2.0 do not shift context (in 1.0
they do ???)

partial is just a template file component can be just a template file

include partial with {{partial 'foo'}}

include component with {{foo}}

partials are in same scope as the thing that contains them componets are not (by
design they create a new scope) and need to have the variables they need
injected

# Should we be able to have partials in a pod?

What does this get us that components do not?

++ current setup forces us to just have one template per pod which is bad if you
have a lot of content
