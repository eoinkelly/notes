# acts-as-taggable-on

```ruby
class Foo < ActiveRecord::Base
  acts_as_taggable_on :c1, :c2
end

@foo = Foo.new
@foo.c1_list
@foo.c2_list

@foo.c1 # all the c1 tags on @foo

Foo.tagged-with("some-value', on: :c1) # find Foos' based on a tag
```

* each cX is a "tagging context" - a namespace for a set of tags
* This lets you use tags for different purposes
