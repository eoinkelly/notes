# Ruby type conversions

There are 2 types of methods:

1. strict = implicit conversion methods
    - used by ruby core classes
    - called automatically by ruby to convert type into the required type when
      the target is expected to act exactly as a particular type
    - examples
        - #to_str is used by + (string concatenation operator)

2. non-strict = explicit conversion methods
    - #to_s (used by string #{interpolation})
    - almost every core object has a #to_s method implemented because almost any
      object can get some sort of string representation

# Key idea

- #to_s is a "string like" representation of the object
- #to*str is telling ruby that this object \_is* string like and that ruby can
  happily do "stringy" things to it.

TODO: more about this in @avdi Confident Ruby
