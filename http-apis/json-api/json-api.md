# JSON API

# Goals

1. minimize number of requests between client & server
2. minimize amount of data transmitted between client & server

# Media type

Both clients and servers must use the official media type to exchange data

    Content-Type: application/vnd.api+json

# Document structure

* objects MUST NOT contain any members not defined in the spec - clients and
  servers MUST ignore any extra properties on those objects

* A JSON _object_ is at the root of every response _containing data_.
* A document must contain at least one of the `data`, `errors`, `meta` members
* `data` and `errors` cannot be in the same document
* Optionally the root object can have `links` or `included`

* `data`
    * the "primary data" i.e. either
    1. the representation of the single resource - one of
        * a single resource object
        * a single "resource identifier object"
        * null
    2. the representation of a collection of resources - one of
        * an array of "resource objects"
        * an array of "resource identifer objects"
        * an empty array
    * If it is a representation of a collection it should _always_ be an array
      (even if empty)
* `errors`
    * an array of error objects
        * what are "error objects" ???
    * each "error object" MAY contain the keys:
        * id
        * href
        * status
        * code
        * title
            * human readable short explanation of what happened
        * detail
            * human readable detailed explanation of what happened
        * source
        * meta
* `meta`
    * non-standard meta information
* `included`
    * an array of "resource objects" that are related to the primary data
      and/or each other
* `links`
    * URLs related to the primnary data

### Terms:

* Resource object
    * seems to be an ordinary JSON object with data in it
    * must contain `id` and `type`
        * `id` can be missing if object came from client and isn't known yet
    * may contain:
        * `attributes`
            * a _single object_ containing the data attributes of the
              resource
            * the keys in here are conceptually in the same namespace as those in `relationships`
            * forbidden keys in here: `id`, `type`
        * `relationships`
            * a _single object_ which describes relationships between this
              resource and other resources
            * shares a namespace with the `attributes` key so subkeys here must have different names to those in attributes
            * top level keys in the relationship object are the names of relationships
            * contains more or more "relationship objects"
            * a _relationship object_:
                * contains at least one of `links`, `data`, `meta`
                * `links` contains at least one of
                   * `self` = a URL for the _relationship itself_. This allows
                     the client to directly manipulate the relationship itself
                     e.g. delte the relationship without deleting any of the
                     objects involved
                   * `related` = a "related resource URL" when this URL is
                     fetched it provides a response where that resource is the
                     primary data
                * If your resource (not necessairly the underlying model) has
                  an association then it must be represented by a relationship
                  even if the value of the URL is `null` or `[]` for an
                  empty-to-one or an empty-to-many relationship respectively
        * `links`
            * urls related to the resource
        * `meta`
            * info about the resource that cannot be represented as attributes
              or relationships
    * JSONAPIResources does not seem to use the attributes object ???

* Resource identifier object
    * a JSON object that functions as a pointer to an object with data e.g.
    ```json
    {
        "data": {
            "type": "posts",
            "id": 33
        }
    }
    ```
    * it seems that URLs that go through `links/` return RIOs not ROs
    * if you follow the links URLs you get these "pointers", not the real
      (presumably heavier) data objects

Things which are common to both resource objects and resource identifier objects:
* type
    * can be plural or singular but should be consistent either way
* id

Links object

A links object can appear at many levels e.g.

* top level of document
* within a relationship object
* within a resource object

but they always have the same form:

* self
* related
* pagination

There are two formas of an individual "link" object

```js
// form 1:
"links": {
  "self": "http://example.com/posts",
}

// form 2:
"links": {
  "self": {
    "href": "http://example.com/posts",
    "meta": {}
  }
}
```

### Conclusions about document structure

* top level document
    * 5 possible top level properties: data, errors, meta, links, included
    * data and errors property cannot be in same document
* each resource object
    * must: id, type
    * may: attributes, relationships, links

If you ask for a resource that does not exist the server should give you a
response with `null` or `[]` in the data key - it should not give you a 404

# Asking for related resource

GET /articles/1?include=comments
GET /articles/1?include=comments.author
GET /articles/1?include=author,comments.author

# Sparse fieldsets

The client can request that the server only return a subset of the available
fields in each response

GET /articles?include=author&fields[articles]=title,body&fields[people]=name

# Sorting

Server MAY support sorting of data as requested by the client. If the server
does not support sorting it should return `400 Bad Request`

    GET /people?sort=age
    GET /people?sort=age,name
    GET /articles?sort=-created,title

Prefix the field name with `-` to sort descending

# Pagination

Server may paginate data. If it does it must include a links object with the keys

* first
* prev
* next
* last

The `page` query parameter is reserved for pagination

# Filtering

The spec has reserved the "filter" query parameter for filtering but doesn't
have much else to say about it.

# Creating resources

* Server may allow client to CUD resources
* If the client supplies an id for a resource it SHOULD be a UUID. This isn't
  locked down to a MUST as there are some edge cases where you might be
  importing data that has some other globally unique naming scheme but in
  general client created `id`s should follow UUID spec
* Updating resources happens over a `PATCH` request. Server should interpret
  missing attributes of an object as not needing change (not that those
  attributes should be set to `null`)
* Relationships may be updated by either updating the objects involved or
  sending `PATCH` request to the relationship URL
