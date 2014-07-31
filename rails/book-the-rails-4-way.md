# The Rails 4 Way

## Chapter 1

* bundler does dependency resolution all at onece
* ruby gems does it one at a time

### The Gemspec file

* NB: Rails 4 no longer has a 'assets' group

* Bundle supports git repos without a `.gemspec`
* Bundler support getting multiple gems out of the same repo (if it has multiple gemspecs in its root)

```ruby
# bundler supports lots of ways to load gems

# git
gem 'foo', '1.1', git: 'git://github.com/foo/foo.git' # give version if no .gemspec file
gem 'foo', github: 'git ...' # github repos are directly supported
gem 'foo', require: 'foo/blah' # if gem name differes from what require statement needs
gem 'foo', git: '...', ref: '00023', branch: '4.4-foo', tag: 'hi-there'

# load from filesystem
gem 'foo', path: '~/path/to/gem'
```

`bundle install`
* _updates_ all dependencies named in the Gemfile to the latest versions that do not conflict with other dependencies
* writes out its results into `Gemfile.lock`
* you can skip certain groups using `--without`
    `$ bundle install --without development test`

`bundle show`
* shows _where_ a bundled gem is installed

`bundle update`
* ???

`bundle package`
* packages all the gems in the `vendor/cache` dir
* use with `bundle install --local` which will pull your gems from here
    * allows you to use private gems in production
    * allows you to avoid external dependencies at deploy time


Q: Why can I sometimes get away with not having to do bundle exec?

### Binstubs

* scripts in `/bin` that run built-in rails command line tools (bundle, rails, rake, spring)
* saves you having to do `bundle exec foo` each time
* they should be added to git
* you can add your own via `bundle binstubs name-of-gem`

### Spring

* https://github.com/rails/spring
* it is what runs when you type `rake|rspec` etc. on the console
* uses rails class reloading to reload files when they change
* is smart enough to restart the app if you touch rails boot/config files
* it does the magic reloading in the test environment as well as development
* it polls the filesystem for updates every 0.2 seconds

Gotchas
* don't save reference to constants in app initialization code as they will get different object_id when reloaded
