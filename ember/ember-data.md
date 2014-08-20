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
* irregular keys can be mapped with a custom serializer


