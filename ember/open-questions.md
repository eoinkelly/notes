
# Is it good/bad for ember models to not match the server models?


# Manage form validation by having a no. of "states" taht the form goes through e.g.

* initail
* editing
* loading
* failure
* success

Usage

* keep these states as an attribute of the component that manages the form
* have action handlers set the state depnding on what happened in the DOM
* you can apply these states as classes on the component element and use them to scope your CSS

# Investigate didAppear hook of views

> Need to handle view transitions, in the visual sense?  Define a mixin that implements two methods: didInsertElement (be sure to call this._super()!), which in turn calls didAppear.  Each view’s didAppear hook can then set up any transitional properties necessary to animate the view appearing.

# Investigate why didTransition isn't called on route change sometimes ???



# Idea: ember-vuvuzela

* a lib you load that overrides the normal ember objects and outputs console log whenever their hooks are invoked
* the idea being that you can turn this on as a newb and see what is happening in your app.
