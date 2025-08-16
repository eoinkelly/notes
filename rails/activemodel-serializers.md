# ActiveModel Serializers

- Will serializer any object that implements `read_attribute_for_serialization`
    - ActiveRectord and ActiveModel objects do

it knows what model it relates to by its own class name e.g. `FooSerializer` is
a serializer for `Foo` you specify the attributes you want to serialize using
`attibutes` method

- if a symbol you pass to `attributes` matches a method in the serializer object
  it will invoke that instead
- the underlying model is available as `object`
- it has built in support for adding a `meta` keyword to the JSON object
