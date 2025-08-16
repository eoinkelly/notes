# 227

- `begin ... end` can be used to group statements in ruby
    - they are a general purpose langauge feature
    - they are not _just_ for exceptions!
    - the return value is the value of the last statement
    - it does _not_ introduce a new variable scope

```ruby
# make a do-while with begin
# see ep 73
begin
  # stuff
  # stuff
end while thing.is_true?

# computing a value with begin
foo = begin
  # stuff
  answer
end

# memoizing a value that takes multiple lines to compute with !!= and begin
foo ||= begin
  # stuff
  # stuff
  # stuff
  answer
end


  @memo ||= get_a_thing
end

# the non-memoized version (in case you need to force the computation)
def get_a_thing
  # * compute the thing and return it
  # * Do not assign to @memo in here - if the user wants to run #get_a_thing and
  #   also fill @memo they can just run #a_thing
end
```

QUESTION: how exactly does ||= work?
