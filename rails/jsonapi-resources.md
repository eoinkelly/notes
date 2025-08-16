- each resource object is usually backed by a model
    - usually an ActiveModel::Model
    - the model is available in the resource object as @model
- the resource has the same context as the controller so you can use
  `current_user` etc.

    app/models/foo.rb app/resources/foo_resource.rb

does model need to be activemodel?

- resources can be declared `abstract` which means they don't have a model
- resource can be `immutable` which means it doesn't generated writable
  endpoints

- the generator does not add any routes

Definigin routes

```
# notice it uses the plural
jsonapi_resources :contacts
```

- attributes can come from @model properties or methods in the resource
- resource is a kind of decorater object for the model
- Available attributes must be declared
    ```ruby

    ```

# declare single attribute

    attribute :foo

# declare multiple attributes

    attributes :foo, :bar
    ```

- all attributes are read-write by default

- `self.fetchable_fields`
    - override to say which attributes can be read
- `self.updateable_fields`
    - override to control which attributes are writeable
- `self.creatable_fields`
    - override to say which attributes can be created
