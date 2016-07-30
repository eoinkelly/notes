## Specific RSpec features

In general I think I am favoring using the minimum amount of rspec sugar - it's
just more stuff that a reader has to understand to grok the tests.

### subject

* -- I think it throws away an opportuntiy to use a better name for the subject
  e.g. `foo_with_some_val`
* ++ It is quite a descriptive name I guess as long as it is clear exactly what the subject is
* -- just some sugar syntax that doesn't add much value - `let(:subject) { ... }` is clearer

### On let and let!

* be careful not to use them to make specs too dry
* ++ let is very convenient
* -- let is most convenient if you are creating slow collaborators that you want to memoize
    * might be much less of an issue if test style is mockist ???
* -- let! is less useful, might be better in a before block ???

### rspec custom matchers

* ++ they do read nicely and make the test less noisy
* -- they hide the detail of how the test passed/failed
* -- they hide an opportunity to show to to introspect the object
* -- not likely the reader will know exactly how they work without searching

### shared examples

* ++ good for behaviours that are common to 2+ methods

# naming of doubles

* I now prefer naming doubles without a prefix e.g. `foo` not `fake_foo` or `foo_double`
    * the suffix seems noisy and unnecessary - it shouldn't matter in the
      context of the test whether the collaborator is real or not

# On DRY in tests

* if code is not in the it block in question where is it acceptable for it to
  be
    * in same file
        * before block
    * in other file
        * as shared module
        * as custom rspec matcher

what are pros/cons of moving functionality out of the it block???

* I now believe that tests should not be dried up outside the boundary of the
  method level describe block. Reasons:
    * test readers will start at the it block but it is probably overkill to
      repeat *everything* in each it block
    * moving code outside of this is tempting but makes the tests less "local"
      so should happen rarely

