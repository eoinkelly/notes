| Rspec name | Rails name |
| ---------- | ---------- |

Q: Since rails has deprecated "single controller" functional tests, should we
not use Rspec controller specs and use rquest specs instead?

Rspec spec types

1. Model specs
    - `spec/models`
2. Controller specs
    - `spec/controllers`
3. Feature specs
    - `spec/features`
    - drives the app with Capybara
    - the docs don't reference any built-in Rails equivalent
4. Request specs
    - `spec/requests`
    - are a thin wrapper around Rails' integration tests
    - Are the closest to Rails controller testing as documented in the Rails
      guide
    - rspec provides 2 matchers which delegate to rails assertions
        ```
        render_template # delegates to assert_template
        redirect_to     # delegates to assert_redirected_to
        ```
5. View specs
    - `spec/views`
6. Helper specs
    - `spec/helpers`
7. Mailer specs
    - `spec/mailers`
8. Routing specs
    - `spec/routing`
9. Job specs
    - `spec/jobs`
10. System specs
    - `spec/system`

Rails provides the following classes for you to inherit from:

1. Model tests
    - Test class inherits from `ActiveSupport::TestCase`
2. System tests
    - live in `test/system`
    - Seem similar to Rspec feature specs
    - Test class inherits from `ActionDispatch::SystemTestCase`
    - Drives your app with a real or headless browser
    - By default, system tests are run with the Selenium driver, using the
      Chrome browser, and a screen size of 1400x1400
    - I think they pull in capybara - TODO: check this
    - Example:

        ```ruby
        require "application_system_test_case"

        class UsersTest < ApplicationSystemTestCase
        # test "visiting the index" do
        #   visit users_url
        #
        #   assert_selector "h3", text: "Users"
        # end
        end
        ```

3. Mailer tests
    - Test class inherits from `ActionMailer::TestCase`
4. View tests
    - Test class inherits from `ActionView::TestCase`
5. Job tests
    - Test class inherits from `ActiveJob::TestCase`
6. Integration tests
    - Test class inherits from `ActionDispatch::IntegrationTest`
    - can test just one controller endpoint or multiple controller endpoints in
      the same test
7. Generator tests
    - Test class inherits from `Rails::Generators::TestCase`

## Controller tests

- live in `spec/controllers` (note the plural)
- A controller spec is an RSpec wrapper for a Rails **functional** test
  `ActionController::TestCase::Behavior`

- The Rails testing guide has separate sections for "Integration tests" and
  "Functional tests" but the examples in both cases inherit from
  `ActionDispatch::IntegrationTest`
    - integration tests are scaffolded into `test/integration`
    - functional/controller tests are scaffolded into `test/controllers`

There are two "styles" of controller test

1. integration style
    - Test classes inherit from `ActionDispatch::IntegrationTest`
    - these tests **actually perform requests**
    - encouraged by Rails
    - are as fast as "functional style" tests
    - ++ provide extra helpers for testing controller actions e.g.
        - `parsed_body`
1. functional style
    - these tests **simulate performing requests**
    - should only be used for backwards compatibility
    - deprecated in Rails

The current Rails guide shows controller tests inheriting from
`ActionDispatch::IntegrationTest`

```ruby
# Rails model test
class ArticleTest < ActiveSupport::TestCase
end
```

```ruby
pry> ActiveSupport::TestCase.ancestors
  [
    ActiveSupport::Testing::SetupAndTeardown,
    ActiveSupport::TestCase,
    ActiveRecord::TestFixtures,
    ActiveSupport::Testing::FileFixtures,
    ActiveSupport::Testing::TimeHelpers,
    ActiveSupport::Testing::Deprecation,
    ActiveSupport::Testing::Assertions,
    ActiveSupport::Callbacks,
    ActiveSupport::Testing::TaggedLogging,
    Minitest::Test,
    Minitest::Guard,
    Minitest::Test::LifecycleHooks,
    Minitest::Reportable,
    Minitest::Assertions,
    Minitest::Runnable,
    ActiveSupport::ToJsonWithActiveSupportEncoder,
    Object,
    PP::ObjectMixin,
    ActiveSupport::Tryable,
    ActiveSupport::Dependencies::Loadable,
    JSON::Ext::Generator::GeneratorMethods::Object,
    Kernel,
    BasicObject
  ]

[7] pry(#<PagesController>)> ActionDispatch::IntegrationTest.ancestors
=> [ActionDispatch::IntegrationTest,
 Rails::Controller::Testing::TestProcess,
 Rails::Controller::Testing::Integration,
 Rails::Controller::Testing::TemplateAssertions,
 ActionMailer::TestCase::ClearTestDeliveries,
 ActionDispatch::IntegrationTest::UrlOptions,
 ActionDispatch::Routing::UrlFor,
 ActionDispatch::Routing::PolymorphicRoutes,
 ActionDispatch::IntegrationTest::Behavior,
 ActionController::TemplateAssertions,
 ActionDispatch::Integration::Runner,
 ActionDispatch::Assertions,
 ActionDispatch::Assertions::RoutingAssertions,
 ActionDispatch::Assertions::ResponseAssertions,
 Rails::Dom::Testing::Assertions,
 Rails::Dom::Testing::Assertions::SelectorAssertions,
 Rails::Dom::Testing::Assertions::SelectorAssertions::CountDescribable,
 Rails::Dom::Testing::Assertions::DomAssertions,
 ActionDispatch::TestProcess::FixtureFile,
 ActiveSupport::Testing::SetupAndTeardown,
 ActiveSupport::TestCase,
 ActiveRecord::TestFixtures,
 ActiveSupport::Testing::FileFixtures,
 ActiveSupport::Testing::TimeHelpers,
 ActiveSupport::Testing::Deprecation,
 ActiveSupport::Testing::Assertions,
 ActiveSupport::Callbacks,
 ActiveSupport::Testing::TaggedLogging,
 Minitest::Test,
 Minitest::Guard,
 Minitest::Test::LifecycleHooks,
 Minitest::Reportable,
 Minitest::Assertions,
 Minitest::Runnable,
 ActiveSupport::ToJsonWithActiveSupportEncoder,
 Object,
 PP::ObjectMixin,
 ActiveSupport::Tryable,
 ActiveSupport::Dependencies::Loadable,
 JSON::Ext::Generator::GeneratorMethods::Object,
 Kernel,
 BasicObject]


```

```ruby
# spec/controllers/articles_controller_spec.rb
RSpec.describe ArticlesController, type: :controller do
end

# is comparable to ...

# test/controllers/articles_controller_test.rb
class ArticlesControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get articles_url
    assert_response :success
  end
end
```

Given/when/then in a controller test is

- Given: same as other tests
- When: make a request using one of the request helper functions (`get`, `post`
  etc.) - this populates a number of ivars and methods (see below)
- Then: see below

```
# Inside a controller test you have access to the following **after** the request is made:

    flash['foo']
    session['foo']
    cookies

    @controller # the controller processing the request
    @request # the request object
    @response # the response object
```
