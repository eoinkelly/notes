# 3 thing to test in a model

1. specify each attribute (by specifyting what represents valid/invalid data)
2. specify each business rule that the model enforces
3. specify each edge cases these should initially be edge cases on the
   happy-path these are not edge cases looking for malicious input/odd cases

# 1. Testing Validations

- start with a spec that shows how to make a valid model - it should show the
  **minimum** required to make a valid model - re-use this model for all the
  attribute tests - it is your "valid model baseline"

    it "is valid with valid attributes" it "is not valid without foo" it "is not
    valid without bar" ...

## ?? should i test validations, if so how?

for they cuase a lot of problems if misconfigured you are testing that they
exist in a particular model, not that they work right (we assume they do) if i
implement a custom validator I must test it

against

- they are just declarative statements in rails this is negated by us not
  testing _how_ they work, just that they exist and are configured correctly

??? should i test that the validity errors we get back are correct?

options

- shoulda matchers - make it very short & easy
    - does not test some extra args to validations???
- manually: specify a valid model, then invalidate in one way in each example
    - not full coverage
    * you get to name it nicely
- set an expectation that they validation method should be called (test
  behaviour not state)
  http://blog.jayfields.com/2006/12/rails-unit-testing-activerecord.html
-
- for each attribute, test each validation by creating a valid model and then
  explicitly invalidating one attribute - this ensures that we are failing on
  the correct attribute

# 2. Testing other model methods (that enforce business rules)

for non validations, teh book seems to scope at the method level then cases
within it

an example:

    ModelName
      #some_method
        In the case where the user is logged in
          x happens
          y should not be allowed
        In the case where the user is ....

    all the cases above should be happy-path

when creating your it "blah blah" pending examples, just follow the happy-path -
don't try to do exhaustive testing at this stage

# Key rspec tests for models

- be_valid

- errors_on(:attr_name) model.should have(:no).errors_on(:foo)
    - calls #valid? on the model

- error_on model.should have(1).error_on(:bar)
    - calls #valid? on the model

- records | record
- model.should have(1).record
- model.should have(:no).records
- model.should have(11).records # warning jim weirich does not like this calls
  #find(:all) on the model

# Tips

- name your variables as @variable so you can later refactor them into
  before(:each) block without renaming

### ways of disconnecting your test from the DB

- NullDB gem
- UnitRecord gem but sometimes you will want to hit the DB to be sure that it is
  working as expected ? really? does integration test not cover this?

### test data builder pattern

- factory girl separate construction of the object from it's representation so
  the construction process can be re-used.

they let your example override the bits that it cares about so you can see what
is important easily form the test code - similar to the pattern we use in the
validator above examples of gems that allow this pattern _ factory-girl _
object-daddy _ machinist _ fixjour

### custom macros

see chap 24 on controllers think they are refering to shared examples but not
sure - check this!

## ??? Testing associations

Should i test associations?

- rspec book says no. it says:
    - assocations are tested _indirectly_ when you test your methods that use
      them
    - _assocations should not be added unless they are serving the needs of some
      behaviour_ (eoin: behaviour == model method ??)
    - so if we never add an association that we are not immediately about to
      use, we can be sure that it won't be untested
    - also says not to test association options (foreign_key, class_name) as
      they are a structural part of the association

* shoulda makes it easy to

If my model's code does not use the association, should I have it there at all?
? is there a cost to extra assocations? ? extra complexity for the reader

When testing associations, use #mock_model to create whatever associated objects
you need this means you don't have to really create the other models to run your
tests

### ??? nested assocations

things that are not being tested

- that connected models get destroyed when we get destroyed (if dependent =>
  destroy is set)
- that we can accept_nested_attributes for our connected models this is
  different to testing the association

# ??? Testing Mass Assignment

Should I test mass assignment in model unit tests?

- shoulda makes it easy
- sometimes there is documentation value in saying _why_ you enabled or disabled
  a particular mass assigment

* theoretically mass assignment should be tested by a higher up test somewhere
  (controller/integration etc.)
    - but that might not happen
    * if you don't make an association until you are actually using it in some
      code, your mass assignments should always have tests covering them
    - which is ok for TDD apps but what about an exisitng one?
