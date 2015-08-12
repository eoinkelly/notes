# Misc stuff from ruby tapas
```ruby
# he uses inline assigns - ugh?
gateway = instance_spy("SomeGateway", do_thing_func: result = double('the result'))


# interesting rspec matcher stuff - should dig more into this
expected_keys = {
    foo: "blah",
    bar: "boo"
}
expect(thing.foo).to match(hash_including(expected_keys))
```

rspec spy and instance_spy

is using RSpec.describe a useful thing?

* `double` `double('some_name')`
    * every rspec double is implicitly a spy i.e. it records the messages it receives
    * use the `have_received` matchers to assert
* spy
    * just a shortcut for `double.as_null_object`
    * will happily receive any message
* intance_spy("SomeClass")
    * will only accept messages that are defined on `SomeClass`


it is tricky to verify the absence of a message send
rspec has introduced "verifying doubles" - instance_spy is one of these

instance_spy("SomeModel") # will error if you send it a message that SomeModel does not implement

# Handling exceptions in RSpec

```ruby

# not great
expect { thing.foo }.to raise_error # too vague, any rspec or type could pass this
expect { thing.foo }.to raise_error("exact error message") # too brittle, coupled to the exact text of the exception message

# better
expect { thing.foo }.to raise_error(/exact error message/)

# best
class Thing
    # generally don't do this - only do it for truly catastrophic cases where you are ok if app dies
    # class CustomError < Exception; end

    # ruby non-fatal exceptions generally inherit from StandardError so we should too
    # "Error" is a good name because it is already namespaced within our class
    class Error < StandardError; end

    def foo
      # we are inside Thing so Error can be used without prefix
      fail Error, "bad thing happened"
    end
end

expect { thing.foo }.to raise_error(Thing::Error)
```


# Converting between numbers and strings

```rb
# number to string
#
42.to_s # => "42" a string of the decimal (base 10) representation
42.to_s(2) # => "101010" a string of the binary representation
42.to_s(16) # => "2a" a string of the binary representation


# string to number
#
"FFEE".to_i(16) # returns 0 if it fails
# Integer is better as it will raise error if it fails
Integer("FFEE", 16)
```
