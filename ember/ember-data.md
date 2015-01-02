# Ember data

Existing docs:

* http://emberjs.com/blog/2014/03/18/the-road-to-ember-data-1-0.html
* http://codebrief.com/2013/07/10-things-you-can-do-with-epf/
* http://emberjs.com/guides/models/

Alternatives:

* http://epf.io/
    * diff: has no state machine backing each model
    * supports REST backends, streaming still TODO
    * last touched july 2014
* Plain old $.ajax
    * Discourse uses it.
    * QUESTION: Where is its sweet spot?
    * QUESTION: how to use it in such a way that you can grow into ember-data or others?
* Ember-model https://github.com/ebryn/ember-model
    * Intentional limited feature set - provides primitves on $.ajax
    * created by @ebryn
    * 483 stars, last touched very recently
* Emu https://github.com/charlieridley/emu
    * Untouched since oct 2013 - ignore

Size comparison of the alternatives

 metric     | $.ajax    | EPF   | Ember-model   | Ember-data
 -----------|-----------|-------|---------------|-----------
 lines      | 0         |       | 2036          | 12944
 bytes      | 0         |       |               | 394 kb

# Features to compare on

What are the things that we should compare persistence frameworks against each other:

* A model API for the rest of the app to use.
    * ideally this should wrap all persistence related stuff so the persistence layer could be changed easily
    * QUESTION: how easily is it to match the semantics of the different peristence options?
* side loading
* relationships
    * embedded or "reference by id"
        * is having both a bad thing?
* how does it handle asynchrony
    * probably making everything return a promise all the time is a good idea
* performance
    * speed
    * memory usage
* REST and/or real-time
    * Is hiding this as an implementation detail form the rest of the app a good idea?

### The Store

* there is exactly one instance of `DS.Store` in the app
* models do **not** have data themselves - they just define the structure
    * is it bad/even possible to add default values to models ??
* you refer to it:
    * it is available as `this.store` in route objects
* memoizes records
    * the second time you load a record, the store will give you the cached version
    * you will get back the *exact same instance* that you got the last time
    * this feature is known as "identity map"
    * it means that any changes you make in one part of the app will be
      propagated to others - this is much easier than managing multiple
      instances of a record in memory at the same time
* `find()` always returns a promise immediately

### Records (instances of models)
* are instances of models
* identified by

1. a **globally unique** id and
    * can be generated client side.
        ???
2. model name

### The Adapter

* system has a single apdater
* it is a translator
* translates calls from the model layer into the appropraite storage/server calls
* the system will ask the adapter for models that it doesn't have cached already

### The Serializer

* converts between JSON and record objects (instances of models)
* could also convert between binary data if you needed it to


### Model naming

photo           App.Photo
userProfile     App.UserProfile

### Attributes

attributes `DS.attr()` control how the JSON string representation is turned into a real JS object (and vice versa)
* passing a type to `DS.attr()` controls what kind of property will be corerced into
* default attrs: string, number, boolean, date


# relationships
hasMany relationship expects an array of ids in the JSON
belongsTo expects a single id

You
You can add computed properties to models that depend on primitive properties

You can sideload more records in the same JSON response

### RestAdapter

```
namespace: '/api/v1',
host: 'http://www.foo.com'

var post = store.find('post', 1); // GET /posts/1

// for odd pluralization
Ember.Inflector.inflector.irregular('formula', 'formulae'); // GET /formulae/1 not GET /formulas/1
Ember.Inflector.uncountable('advice'); // ???
```

Expects JSON:
* camelCased attribute names
* the primary record being returned should be in a named root e.g. `GET
  /people/12` should return
  ```
  { person: {
      name: 'Eoin'
    }
  }
  ```
* irregular keys can be mapped with a custom serializer


# How do you tell the store to empty records from its cache?

store.unloadAll('post'); unload all records of type 'post' from the store

store.unloadRecord(record);

record.transitionTo('deleted.saved');
