# How to create an engine

```
rails plugin new ENGINE_NAME --dummy-path=spec/dummy --skip-test-unit --mountable
TODO: convert to rspec
    See
    https://www.viget.com/articles/rails-engine-testing-with-rspec-capybara-and-factorygirl
TODO: edit gemfile
TODO: remove MIT license if appropriate
```

# Engines background

Rails::Application inherits from Rails::Engine \* => you can think of a rails
app as a specialisation of an engine

- engines have same directory layout as plugins engine routes are namespacedk
  controllers, models and table names are namespaced within the engine host
  application code always takes precedence

```
$ rails plugin foo            # generates a plugin named foo
$ rails plugin --full         # generates an engine named 'foo'
$ rails plugin --mountable    # generates an engine named 'foo' with extra stuff ???
```

```
$ rails plugin new plugin_no_args
      create
      create  README.rdoc
      create  Rakefile
      create  plugin_no_args.gemspec
      create  MIT-LICENSE
      create  .gitignore
      create  Gemfile
      create  lib/plugin_no_args.rb
      create  lib/tasks/plugin_no_args_tasks.rake
      create  lib/plugin_no_args/version.rb
      create  test/test_helper.rb
      create  test/plugin_no_args_test.rb
      append  Rakefile
  vendor_app  test/dummy
         run  bundle install
You have one or more invalid gemspecs that need to be fixed.
The gemspec at /Users/eoinkelly/Desktop/plugin_no_args/plugin_no_args.gemspec is not valid. Please fix this gemspec.
The validation error was '"FIXME" or "TODO" is not a description'


rails plugin new --full plugin_full
      create
      create  README.rdoc
      create  Rakefile
      create  plugin_full.gemspec
      create  MIT-LICENSE
      create  .gitignore
      create  Gemfile
      create  app/models
      create  app/models/.keep
      create  app/controllers
      create  app/controllers/.keep
      create  app/views
      create  app/views/.keep
      create  app/helpers
      create  app/helpers/.keep
      create  app/mailers
      create  app/mailers/.keep
      create  app/assets/images/plugin_full
      create  app/assets/images/plugin_full/.keep
      create  config/routes.rb
      create  lib/plugin_full.rb
      create  lib/tasks/plugin_full_tasks.rake
      create  lib/plugin_full/version.rb
      create  lib/plugin_full/engine.rb
      create  app/assets/stylesheets/plugin_full
      create  app/assets/stylesheets/plugin_full/.keep
      create  app/assets/javascripts/plugin_full
      create  app/assets/javascripts/plugin_full/.keep
      create  bin
      create  bin/rails
      create  test/test_helper.rb
      create  test/plugin_full_test.rb
      append  Rakefile
      create  test/integration/navigation_test.rb
  vendor_app  test/dummy
         run  bundle install
You have one or more invalid gemspecs that need to be fixed.
The gemspec at /Users/eoinkelly/Desktop/plugin_full/plugin_full.gemspec is not valid. Please fix this gemspec.
The validation error was '"FIXME" or "TODO" is not a description'


$ rails plugin new --mountable plugin_mountable
      create
      create  README.rdoc
      create  Rakefile
      create  plugin_mountable.gemspec
      create  MIT-LICENSE
      create  .gitignore
      create  Gemfile
      create  app
      create  app/controllers/plugin_mountable/application_controller.rb
      create  app/helpers/plugin_mountable/application_helper.rb
      create  app/mailers
      create  app/models
      create  app/views/layouts/plugin_mountable/application.html.erb
      create  app/assets/images/plugin_mountable
      create  app/assets/images/plugin_mountable/.keep
      create  config/routes.rb
      create  lib/plugin_mountable.rb
      create  lib/tasks/plugin_mountable_tasks.rake
      create  lib/plugin_mountable/version.rb
      create  lib/plugin_mountable/engine.rb
      create  app/assets/stylesheets/plugin_mountable/application.css
      create  app/assets/javascripts/plugin_mountable/application.js
      create  bin
      create  bin/rails
      create  test/test_helper.rb
      create  test/plugin_mountable_test.rb
      append  Rakefile
      create  test/integration/navigation_test.rb
  vendor_app  test/dummy
         run  bundle install
You have one or more invalid gemspecs that need to be fixed.
The gemspec at /Users/eoinkelly/Desktop/plugin_mountable/plugin_mountable.gemspec is not valid. Please fix this gemspec.
The validation error was '"FIXME" or "TODO" is not a description'
```

- `rails plugin new foo`
- `rails plugin new --full foo`
- `rails plugin new --mountable foo`

- vanilla plugin
    - seems to be mostly just an empty gem with a few handy rails things added
    - it
        - sets up some basic rake tasks: `rake test`, `rake rdoc`
        - seems to be hard-coded to use minitest
        - creates a module for you
        - sets up tests
        - sets up a version number
        - stubs out a gemspec
        - adds a very minimal rails app called "dummy" to the tests dir
- full
    - full engines are not namespaced by default i.e. all their routes,
      controllers, models etc. are mixed into the host app!
    - a full engine is kind of like a mixin rails app
        - might be useful if your host app is super thin or you don't want the
          namespacing
            - I _think_ you can still have unnamespaced routes with a mountable
              engine by mounting it at `/` ???
- mountable
    - a superset of `full`
    - makes the engine "mountable" and adds namespace isolation
    - adds
        - namespaced ApplicationController stub
        - namespaced ApplicationHelper stub
        - layout view template
        -
