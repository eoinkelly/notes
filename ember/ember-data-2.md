both `hasMany()` and `belongsTo()` have options

- defaultValue (undocumented)
    - specify what value the relationship should have before the other model is
      loaded
- inverse
    - says which relationship is the inverse of this one in the other model
    - only needed when you have multiple relationships between two models
    - Q: does ED error if you need this and don't have it?
- async
    - defaults to true since ED 2.0
    - Q: what happens if i set it to false?

Adapter job: determine the URLs

Serializer job: transfrom between Server-JSON <--> ED-model

Adapters

- adapter files are named with singular of the model name
    - `app/models/foo.js` goes with `app/adapters/foo.js`
- the adapter has a bunch of methods with the same names as those in DS.Model
    - they are not the same methods! (Usually the DS.Model version will end up
      calling the adapter version)

Serializers

- serialize()
    - prepare data for server
- normalize()
    - prepare data for ember-model

How to make a model whose primary key is not `id` work:

- add the `primaryKey: "fieldName"` to the **serializer**

What field does ember data use to
