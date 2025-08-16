# reading status

read the recipes book but no notes taken read thoughtbot book up to
file:///C:/Users/Eoin/Desktop/Backbone.js%20On%20Rails%20-%20Thoughtbot/book.html#using-rails-url-helpers-from-backbone

## From Reading the Backbone Source

// Alias the assumed global scope var root = this;

- Declare a local variable and also a version attached to the global scope at
  the same time.

    var foo = root.foo = {};

- both foo and root.foo refer to the same object in memory. Any changes made to
  one will also be made to the other

## Backbone Models

backbone models can do validation! - just add a validate method to the model and
it will be passed the

attributes that are about to be updated

- return nothing to indicate succes, any other return value indicates failure

### Backbone.Model.save()

- model.save({title: 'foo'}, { error: function () {}, success: function () {});
- The save() method can take an error and success callback.
- Failed validation will call the error callback (if one is defined)

Thoughtbot recommend seeding your models with data in the HTML to improve
performance

## Not leaking sensitive data over the wire to the client

- Whitelist which rails model attributes will appear in the JSON. #as_json has
  heaps of handy options

Backbone does not expect a root node in the JSON it gets - it looks like a bit
of a PITA to change this so best

to go along with it. You can do it if you need to - override parse(), toJSON()
in the Backbone.Model and parse()

in Backbone.Collection

## Object organisation

Thoughtbot recommend the following app structure:

var data = { // some seed data filled in by Rails // be careful if you are
dumping user-generated content here (XSS issues) };

var App = { Models: {}, Collections: {}, Views: {}, Routers: {}, initialize:
function () { var tasks = new App.Collections.Tasks(data.tasks); new
App.Routers.myRouter({tasks: tasks); Backbone.history.start(); } };

App.initialize();

/\* My opinions on this:

- They don't use an IIFE for the main App object
    - less code
    * no private variables possible - everything in Models, Collections, Views
      etc. is public \*/

## Models

If you need to HTML escape the data you get from the model, backbone provides
model.escape('attr') as an

alternative to model.get('attr')

## Views

The view populates it's own .el attribute but it is better for something outside
that view to actually place it

into the DOM - this implies:

- parent views insert child views ? should they also introduce the child views
  to the collection/model that they manage?
- the router inserts top level views and tells it what collection/model to
  manage (injects the

dependency)

? views are _often_ bound to a model. Should they always be? What are examples
where they should not be?

- A view's response to a change in model data should be to re-render all/part of
  itself (the model-->view path)
- A view's response to a DOM event might be to change it's models data - it can
  do other things too (the view - -> model path)

? customising the tagname does bind the view more tightly to it's DOM element,
what are pros/cons

- the view now knows what kind of element it is ? is this really a problem - it
  knew it was a div before we customised?
    - the markup is no longer fully in the templates - some of it is in the
      backbone view now

* we don't have any extra container divs in our markup
    - extra container divs breaks stuff like lists (e.g. if the <li> elements
      are each wrapped in <div> )
