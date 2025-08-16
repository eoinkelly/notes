# Notes

I like/use his trick of expressing what you want in english first, then code.

- you can treat strings as an array in ruby!

```ruby
foo = "hello"
foo[1..-1] # "ello"
```

I still don't understand activerecord where() properly yet

## God objects

- breaks single responsibility principle
- tends to attract code - it encourages more of the same
- Me: "code bases encourage more of the same of what they have"

## Using symbol as arg to form_for

```ruby
# the symbol means form_for will not complain if there is not @search object
# If there is a variable anmed @search it will use it, otherwise ???
form_for(:search) do
  # ...
end
```

## Neat pattern for tieing a model to a partial that renders it

He really likes the pattern of using

1. `render @foo` in the view and then

2. make `@foo` act like an ActiveModel
    ```ruby
    class Foo
      extend ActiveModel::Naming
      # ...
    end
    ```
3. create the appropriate partial file

## AR Callbacks and observers

Why are ActiveRecord callbacks bad? Are observers better? Why?

To use ovserver implement an interface (set of methods) on each model that
should be observed have the observer call methods in that interface on anything
it observes

observers - hard ot know when they are active and when they are not new dev
might not notice that extra stuff is happening observer does follow open-closed

To do: research sunspot_rails sunspot_solr

# Questions for Rose

- What is a god object?
    - Why is it a bad thing?
- What does "Not invented here" syndrom mean?
    - If you had to pick a search gem today, which one would you use? Why?
