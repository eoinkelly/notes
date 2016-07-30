# RSpec

## 6 Gems

* RSpec is split into 6 separate gems.
* `gem install rspec` will install them all


1. rspec
2. rspec-core
3. rspec-support
4. rspec-mocks
5. rspec-expectations
6. rspec-rails

## 8 Kinds of tests

There are 8 kinds of spec in rails + rspec:

1. feature specs
    * Make a feature spec:`RSpec.describe 'blah', type: :feature do`
    * Adding `type: :feature` makes some methods available to you in the block
      that help with writing tests from the POV of a browser.
        * these methods are the _Capybara DSL_
    * live in `spec/features` by default
    * manipulates a "fake browser" provided by Capybara
    * _requires_ Capybara. Feature specs are skipped if you do not have Capybara installed!
    * Capybara provides aliases for existing Rspec methods:
        * `feature` for `describe`
            * automatically sets `type: :feature`
        * `scenario` for `it`
2. request specs
    * Make a request spec:`RSpec.describe 'blah', type: :request do`
    * Adding `type: :request` makes some methods available to you in the block
      that help with writing tests from the POV of a browser.
        * these methods are the _Capybara DSL_
    * live in `spec/requests` by default
    * they exercise the full rails stack (routing -> controllers -> models) without stubbing.
    * are a middle-ground between feature specs and controller specs
        * they are not from the POV of a user with a browser - you don't make assertions about the contents of the HTML returned
    * are an alternative to feature specs. Heres how they compare:
        * request specs are faster than feature specs because they don't have
          to use all the "fake browser" machinery
        * request specs are not _true_ end-to-end tests for a normal web page e.g.
            * they don't test any JS you might have on your page
            * they don't make it easy to assert the contents of the DOM in the
              response (they only provide the response as a String object)
        * If you are testing an JSON API then request specs and feature specs
          are much more similar (no HTML DOM, no client-side JS)
    * are a _very_ thin wrapper around the integration testing API provided by
      rails so basically their docs are the rails integration testing API docs
      - see [http://guides.rubyonrails.org/testing.html#integration-testing](http://guides.rubyonrails.org/testing.html#integration-testing)
3. controller specs
    * Make a controller spec: `RSpec.describe 'blah', type: :controller do`
    * Adding `type: :controller` makes some methods available to you in the block
      that help with testing controllers
    * live in `spec/controllers` by default
    * never create HTTP requests
    * do not render views by default
    * you can inspect the instance variables assigned by the controller
      ```
      expect(assigns(:some_ivar)).to eq(some_value)
      ```
4. model specs
    * Make a model spec: `RSpec.describe 'blah', type: :model do`
    * No extra methods available ??? CHECK THIS!
    * live in `spec/models` by default
5. view specs
    * rarely used
6. helper specs
    * rarely used
7. routing specs
    * rarely used
8. mailer specs
    * rarely used


You can just include normal ruby modules in a describe block

```ruby
module UsersSteps
  def sign_in; end
  def submit_form; end
end

RSpec.describe 'blah', type: :feature do
  include UsersSteps
end
```

Aliases:
* describe, context, feature (from Capybara)
* it, specify, scenario (from Capybara)

There are 14 rspec matchers that can take matchers as arguments

Array, String

4. end_with(matcher, matcher, ...)
11. start_with(matcher, matcher, ...)

Array

3. contain_exactly(matcher, matcher, matcher)

Array, Hash, String (anything that responds to #include?

5. include(matcher, matcher, ...)
6. include(:key => matcher, :other => matcher)


1. change { }.by(matcher)
2. change { }.from(matcher).to(matcher)
7. match(arbitrary_nested_structure_with_matchers)
8. output(matcher).to_stdout
9. output(matcher).to_stderr
10. raise_error(ErrorClass, matcher)
12. throw_symbol(:sym, matcher)
13. yield_with_args(matcher, matcher, ...)
14. yield_successive_args(matcher, matcher, ...)

