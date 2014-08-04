# The Rails 4 Way

## Chapter 1

* bundler does dependency resolution all at once
* ruby gems does it one gem at a time

### Rails assets group

* NB: Rails 4 no longer has a 'assets' group
    * gems that were in `assets` are now in production
    * Rails 4 in production will not try to compile assets you forgot to
      precompile (unlike older versions) so there is no reason not to have the
      assets gems in production
    * This is not totally without controversy
* If you do precompile your assets then you don't need the asset compiliation
  gems in production at all
* the rails guys wanted to use coffescript templates in production

The recommended way to precompile assets in Rails 4 is now

```
RAILS_ENV=production bundle exec rake assets:precompile
```

### The Gemspec file

The problem bundler solves:

* A requires any version of B between 3 and 7
* C requires any version of B between 4 and 6
* If A loads first, then it will load B version 7 (the latest that it supports)
  but this will make C sad.
* The old solution was to tweak the load order of your gemfile to make sure C
  loaded first (which would make A happy)



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
* installs to GEM_HOME (same as rubygems) by default
    * this means you will see the gems installed by bundler in rubygems
    * this is convenient for re-using gems
    * QUESTION: where is GEM_HOME on my machine
        * rails seems to be currently: `/Users/eoinkelly/.rbenv/versions/2.1.1/lib/ruby/gems/2.1.0/gems/rails-4.1.1`
    * TODO: learn how to share gems between rbenv rubies (or at least copy them locally - i think I have a lot of dupes

```
# upgrade just production (in normal 3 env setups)
bundle install --without development test
```

* bundle outdated = show outdated gems
* bundle viz = make a PNG of gem dependencies
* bundle show = show info about a single gem
    * shows _where_ a bundled gem is installed

`bundle update`

* ???

`bundle package`

* packages all the gems into the `vendor/cache` dir
* use with `bundle install --local` which will pull your gems from here
    * allows you to use private gems in production
    * allows you to avoid external dependencies at deploy time


### When do I need to use `bundle exec`

* bundler builds a _bundle_ of gems from your Gemfile
* executes the given script in the context of the current bundle
* running the script without using `bundle exec` will work as long as the gem is
  installed and doesn't conflict with any gems in your bundle.

* More on bundler http://bundler.io
* TODO: dig more into bundler - it can do a lot of stuff!

### Binstubs

* scripts in `/bin` that run built-in rails command line tools (bundle, rails,
* rake, spring) _in the context of your current bundle_.
* saves you having to do `bundle exec foo` each time
* they should be added to git
* `rake rails:update:bin` will re-make them (if upgrading from Rails 3)
* you can add your own via `bundle binstubs name-of-gem`
* TODO: currently I seem to be using rbenv stubs when I run commands like
  `rails` - is this OK? should I use local ones instead?

Aside: `rails runner`
  * like rails console but only runs the command you supply on the cmd line
  * e.g. `rails runner "puts User.all.count"`

### Rails boot process

When you boot a rails app there are 3 files responsibilie for setting it up
They are run in the order shown below every time you boot the rails
environment.
The starting point file for a rails app is ???

1. `config/boot.rb`
    * sets up BUNDLE_GEMFILE env variable and runs bundler setup
2. `config/application.rb`
    * requires and runs `config/boot.rb` (so `boot.rb` runs first)
    * creates a module for your app and creates your application class within it
      this is groundwork for running multiple rails apps in the same process in
      future. TODO: find out more
    * loads rails gems
        * here is where you should disable any parts of rails you are not using.
    * runs `Bundler.require` to load gems from the groups in your Gemfile
    * loads gems for the chosen RAILS_ENV
    * configures the app e.g.
        * timezone
        * autoload paths
    * `Rails.groups` is an array of the Gemfile groups (I think. TODO check this)
        ```
        [1] pry(main)> Rails.groups
        [
            [0] :default,
            [1] "development"
        ]
        ```
3. `config/environment.rb`
    * requires and runs `config/application.rb`
    * Runs `Rails.application.initialize!`
    * runs all intializers


### config/initializers

Put any code you need to run at rails boot in here.
There are 7 default initializers:
1. backtrace silencer
2 ...

### Spring

* https://github.com/rails/spring
* it is what runs when you type `rake|rspec` etc. on the console
* uses rails class reloading to reload files when they change
* is smart enough to restart the app if you touch anything in
    * `/config/*`
    * `/config/initializers/*`
* it does the magic reloading in the test environment as well as development
* it polls the filesystem for updates every 0.2 seconds

Gotchas
* don't save reference to constants in app initialization code as they will get different object_id when reloaded

```
spring status
spring stop
spring start
```


### Console

* In Rails 4 you can pass a block to `console` which is a method that is only
  called when you are starting a rails console.
* This means you can tweak your console configuration


### Schema dumping

`config.acitive_record.schema_format = :sql`

When you run tests
1. Rails dumps dev DB schema into `schema.rb` (using migrations API). Think of
   `schema.rb` as one big migration.
2. Rails loads `schema.rb` into the test DB

THis works great except when you have stuff in your dev DB that cannot be
expressed with the Rails Migration API. If you do have custom stuff, you need to
use SQL as the intermediate format i.e. replace `schema.rb` with `schema.sql`

* `schema.rb` is the authorative source of your database *schema* (not data - it contains no data)

good notes up to section 1.2, patchy after that. to do: finish this off
