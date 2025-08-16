# Minitest

- replaced Test::Unit as default test framework bundled with Ruby in 1.9
- backwards compatible API with Test::Unit
- available via `minitest` gem on ruby 1.8

    gem 'minitest', "~> 4.0"

    # Mocking and stubbing library with JMock/SchMock syntax, which allows

    # mocking and stubbing of methods on real (non-mock) classes.

    gem 'mocha', '0.13.3', :require => false

    gem 'webmock', '1.10.0'

    # WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.

    gem 'minitest-reporters'

    # Death to haphazard monkey-patching! Extend Minitest through simple hooks.

minitest/unit minitest/spec minitest/pride minitest/mock

## In rails

```rb
rerequire 'test_helper'

class ArticleTest < ActiveSupport::TestCase
  # test "the truth" do
  #   assert true
  # end
end
```

ActiveSupport::TestCase inherits from Minitest::Test

Rails provides a `test` macro

```
test "things" do
  # ...
end

# gets rewritten by rails into

def test_things
  # ...
end
```

## Assertions

```
assert {some expression that evaluates to truthy or falsy}, {some optional message}
assert_not {some expression that evaluates to truthy or falsy}, {some optional message}
```

# running

Output: . = passing test E = test error F = test failure

Can easily run all specs in a file

    rake test /path/to/test.rb

Stop rails filtering out lines form the backgrace BACKTRACE=1 bin/rake test
test/models/article_test.rb

Cannot easily run just a single test (see m and testrbl)

# run a single test

add something to the test name very unique like:

    it "my test xxx" do
    end

and then use the RegEx version of the '-n' parameter like:

    ruby my_test.rb -n /xxx/
