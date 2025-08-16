# Pundit

- Mostly a naming convention with some glue code

- each _model_ can have a corresponding "Policy" PORO in `app/policies`. If it
  doesn't then the default `ApplicationPolicy` is used
- `app/models/foo.rb` -> `app/policies/foo_policy.rb`
- The policy object is a box which given an instance of the model and a user and
  a "query" (request to take some action) can answer yes/no.
- It can also answer questions like "User X wants to access all Foo objects in
  the system, what subset of them is X allowed to see"
- There is no enforcing of policy outside - your controllers have to acutally
  use the methods for it to work

- The `#authorize` method is use

In controller

Controller `#authorize` method

- used from controller actions
- introspects the class of the model you pass it
- creates an instance of the corresponding policy class
    - passes `current_user` and whatever model instance you proved to
      `#initialize`
- introspects the current _controller_ action name, appends `?` to create the
  name of the "query" you are asking the policy about and calls it as a
  predicate method on the policy instance
    - this method can be overridden by passing a symbol to `#authorize`
- raises an exception if the predicate does not return true

Controller and view: `#policy` helper

- Gets

```rb
authorize(@model_instance)
authorize(@model_instance, :action_name)
```

## scopes

- Each policy has a nested "Scope" class
- Scope is given a user and a scope and when #resolve is called it returns a new
  scope
- The new scope represents what the user is allowed to do e.g. given an AR scope
  `Post` it might return a filtered version of that scope
  `(Post.where(owner: current_user)` which represents which of these models the
  user is allowed to work with
- The default implementation of each Scope class is to just return the scope it
  was given - basically a noop.
- In Rails, "scopes" are usually AR relations but it could be anything as long
  as your Policy object and controller method agree on what to pass in.

```rb
class FooPolicy
  # default implementation of Scope
  class Scope
    attr_reader :user, :scope

    def initialize(user, scope)
      @user = user
      @scope = scope
    end

    def resolve
      scope
    end
  end
end
```
