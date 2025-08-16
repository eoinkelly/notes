# Rails rescue_from

Sources

- http://api.rubyonrails.org/v5.0/classes/ActiveSupport/Rescuable/ClassMethods.html

> Handlers are inherited. They are searched from right to left, from bottom to
> top, and up the hierarchy. The handler of the first class for which
> `exception.is_a?(klass)` holds true is the one invoked, if any.

If you have multiple handlers defined they are searched

1. from right to left
    - you can pass multiple classes to a single `rescue_from` e.g.
      `rescue_from FooError, BarError, BazError, with: ...` the exceptions will
      be checked in the order: BazError, BarError, FooError
1. from bottom to top
    - if you have defined multiple `rescue_from` blocks in a single class they
      will be searched from bottom to top
1. up the heirarchy
    - after that the search continues up the class heirarchy

If you `raise` within a `rescue_from` exception handler it will NOT be caught by
other `rescue_from` handlers i.e. your exception goes through at most one
`rescue_from` handler.

```ruby
class ApplicationController < ActionController::Base

  # rescue_from handlers within the same class are searched from bottom to top
  # so when an exception is raised it will be checked if it is an EoinError
  # before checking it is a StandardError
  rescue_from StandardError, with: :handle_standard_err
  rescue_from EoinError, with: :handle_eoin_err

  def handle_standard_err(e)
    logger.info "handle_standard_err"
    raise e # will not go to another rescue_from handler
  end

  def handle_eoin_err(e)
    logger.info "handle_eoin_err"
    raise e # will not go to another rescue_from handler
  end
end
```
