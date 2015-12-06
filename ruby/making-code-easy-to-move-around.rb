# Making ruby code easy to move around (because refactoring *will* happen) - we want to make code as "copy pasteable" as possible

# * Declare variables just above where they are used.
# * Don't use explicit return statements in methods - this implies methods only have *one* exit point.
# * Put comma after every entry in hash or array

    {
    :foo => "Foo!",
    :bar => "Bar",  # valid Ruby syntax, but sometimes raises PedanticWhingingException when read by other humans
    }

# * If you have many existing calls to a method with X arguments, you can add more as long as you provide defaults for the others e.g.

    do_thing(x, y) # exists in many places in the code

    # option 1
    def do_thing(x, y, z = "foo", alpha = "bar")
        # do stuff ...
    end

    # option 2
    def do_thing(x, z = "foo", alpha = "bar", y) # this also works in Ruby!
        # do stuff ...
    end

# * Put chained messages on different lines to make them easier to 1) re-order and 2) understand

    some_array \
      .map(&:wibble) \
      .sort \
      .reverse \
      .join(', ') # 1.8 way

    some_array
      .map(&:wibble)
      .sort
      .reverse
      .join(', ') # 1.9 way

# I like how it turns this:

    object.something.
            whatever(var).
            last

    object.something
          .whatever(var)
          .last

# Ruby now also affords the ternary operator some similar slack:

    var = test ? if_true
               : if_false

# Apparently this is haskell style

    puts [ one
           , two
           , three
           ]