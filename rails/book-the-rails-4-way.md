# The Rails 4 Way

## Chapter 1

* bundler does dependency resolution all at once
* ruby gems does it one gem at a time

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
* `bundle update`
* `bundle package`
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

* shell scripts in `/bin` that run built-in rails command line tools (bundle,
  rails, rake, spring) _in the context of your current bundle_.
* saves you having to do `bundle exec foo` each time
* they should be added to git
* `rake rails:update:bin` will re-make them (if upgrading from Rails 3)
* you can add your own via `bundle binstubs name-of-gem`
* TODO: currently I seem to be using rbenv stubs when I run commands like
  `rails` - is this OK? should I use local ones instead?

* Aside: `rails runner`
    * like rails console but only runs the command you supply on the cmd line
    e.g. `rails runner "puts User.all.count"`
    * handy for places where you can't run an interactive environment

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


### Other settings you can add to config/application.rb

* Log level override `config.log_level`
* Load path modification `config.autoload_paths`

You can inspect the current configuration in rails console via

```ruby
# MyAppName::Application.config.config_option
MyAppName::Application.config.autoload_paths
MyAppName::Application.config.log_level

# see all available config variables
MyAppName::Application.config.instance_variables

# Example:
[24] pry(main)> MyAppName::Application.config.instance_variables
[
    [ 0] :@root,
    [ 1] :@generators,
    [ 2] :@encoding,
    [ 3] :@allow_concurrency,
    [ 4] :@consider_all_requests_local,
    [ 5] :@filter_parameters,
    [ 6] :@filter_redirect,
    [ 7] :@helpers_paths,
    [ 8] :@serve_static_assets,
    [ 9] :@static_cache_control,
    [10] :@force_ssl,
    [11] :@ssl_options,
    [12] :@session_store,
    [13] :@session_options,
    [14] :@time_zone,
    [15] :@beginning_of_week,
    [16] :@log_level,
    [17] :@middleware,
    [18] :@cache_store,
    [19] :@railties_order,
    [20] :@relative_url_root,
    [21] :@reload_classes_only_on_change,
    [22] :@file_watcher,
    [23] :@exceptions_app,
    [24] :@autoflush_log,
    [25] :@log_formatter,
    [26] :@eager_load,
    [27] :@secret_token,
    [28] :@secret_key_base,
    [29] :@assets,
    [30] :@paths,
    [31] :@autoload_paths,
    [32] :@eager_load_paths,
    [33] :@autoload_once_paths,
    [34] :@cache_classes,
    [35] :@console
]
```

### config/initializers

Put any code you need to run at rails boot in here.
There are 8 default initializers:
1. backtrace silencer
2. Filter parameter logging (tell rails which `params` keys to not log)
3. Inflections
    * Teach `ActiveSupport::Inflector` about new pluralisations
    * You can test inflections in the console:
        * `ActiveSupport::Inflector.pluralize("thing")`
        * `"thing".pluralize` (Inflector is mixed into String)
4. Custom Mime types for use in `respond_to` blocks
    * View all registered mime types: `Mime::EXTENSION_LOOKUP.each { |m| puts m}`
5. session store
    * sets the cookie store type and key
    * session cookies are encrypted with secret key from `config/secrets.yml`
6. Wrap parameters
    * if true, tells `params` to wrap the HTML FORM params it got in a hash
    * keyed based on the controller name e.g.
    ```
    # in NamesController
    { "name": "Eoin kelly" }
    # becomes
    { "name": "Eoin kelly", { "name": { "name": "Eoin kelly"} } }
    TODO: check that this duplication happens!
    ```
    * The key name is the singular of the controller name e.g.
        * FoosController -> "foo"
        * ArticlesController -> "article"
    * by default it is only enabled for JSON ???
7. Assets
    * You can add extra files to be precompiled by the asset-pipeline.
    * You can change the version of your assets
        * which expires them ...
        * QUESTION: what is the use case for this?
8. Cookies serializer
    * Tell rails what format to serialize cookies in (defaults to JSON)

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


When you run tests
1. Rails dumps the development DB schema into `schema.rb` (using migrations API). Think of
   `schema.rb` as a really big migration.
2. Rails loads `schema.rb` into the test DB

THis works great except when you have stuff in your dev DB that cannot be
expressed with the Rails Migration API.
* If you do have custom stuff, you need to use SQL as the intermediate format
  i.e. replace `schema.rb` with `schema.sql`
* If you choose SQL rails will use database specific SQL (so you can't dump from
  Postgres and expect it to work in MySQL).
* You can so this by setting
  `config.active_record.schema_format = :sql`
  in an initializer file within `config/initializers`

* `schema.rb` is the authorative source of your database *schema* (not data - it contains no data)


# `config/environments/development.rb`

* `config.cache_classes`

If true Rails uses ruby `require` to do class loading
If false Rails uses ruby `load` to do class loading

* `require` loads and compiles the file once and then ruby caches the compiled output
* `load` does not cache the compilation so will read the file every time it is referenced


### Rails autoloading

Why don't we have to use `require` statements in rails code?

* Ruby provides a callback mechanism for missing constants
* Rails hooks into this and runs a class loader routine to load a class based on
  naming converntions whenever ruby finds a missing constant
* You can see the dirs that rails will search via `$LOAD_PATH` in console
    * These are mostly
    1. `vendor/`
    2. `lib/`
    3. sub dirs of `app/`
    4. `lib/` directory of each bundled gem
    * Implications:
        * `foo_bar.rb` in `lib/` can be loaded in my rails app via `require "foo_bar"`
        * If my rails app uses `FooBar` rails will load it for me. CHECK THIS


### Eager loading

* `config.eager_load_classes`
    * tells rails to try to load as many classes as possible into memory
      at boot time - not to wait for those classes to be required by a request
    * Rails 4 does not eager load classes in development or test (so the server
      boots quicker) but will do so in production.
`config.consider_all_requests_local`
    * Requests from localhost are the only ones which get more verbose errors
      (stacktrace etc.) but `config.consider_all_requests_local` will send the
      verbose errors to every client.
* `config.action_controller.perform_caching`
    * You can turn caching on to test it in development with
* `config.action_mailer.perform_deliveries`
    * Set `config.action_mailer.perform_deliveries = false` in development to
      have email only be in your log file.
* `config.assets.debug`
    * You can turn off separate asset files in dev via `config.assets.debug = false`
    * This would be handy on slow dev machine it stops sprokets from
      concatenating and minifying files.


QUESTION: how exactly does rails caching work?
  view caching
  asset caching



### Rails Assets

QUESTIONS about assets

* How do I refer to an asset from a view template?
* How do I refer to an asset from my controllers, models, (is it diff for each)?
* How do I refer to asset from sass or JS?

* The asset pipeline arrived in Rail 3.1

URLs to use to check your assets

```
./app/assets/javascripts/application.js.coffee
http://localhost:3000/assets/application.js

./app/assets/stylesheets/application.css.scss
http://localhost:3000/assets/application.css

./app/assets/images/test.jpg
http://localhost:3000/assets/test.jpg

./app/assets/images/foo/test.jpg
./app/assets/stylesheets/foo/test.jpg
./app/assets/javascripts/foo/test.jpg
http://localhost:3000/assets/foo/test.jpg

./app/assets/foo/test.jpg
http://localhost:3000/assets/test.jpg

./app/assets/test.jpg
NOT ACCESSIBLE VIA URL
```

Some rules about how assets work

* `app/assets` is a _source of assets_
* it is expected to contain dirs only.
* the subdir names do not matter to the assets pipeline - they are for humans only
* `app/assets/ANY_DIRNAME/` becomes `http://localhost:3000/assets/`
* `app/assets/somefile.png` is NOT ACCESSIBLE

In production, all precompiled assets are served out of `public/assets`.

Some configuration settings:

* `config.assets.compile = false # rails 4 default`
    * Rails 4 will throw an exception if you ask for an asset that has not been
      precompiled. Older versions would compile it on the fly.
    * Set it to true to use the old behaviour
* `config.assets.enabled`
    * controls whether the asset pipeline is used
    * QUESTION: what do you do if you don't use AP ???
* `config.action_controller.asset_host`
    * Tell rails to serve assets from a different server
    * This is useful for keeping assets on S3 or similar

* TODO: build a toy rails app that serves assets from S3
    * how do you get the assets on there?

QUESTION: what does `config.serve_static_assets` do exactly?
QUESTION: also `config.static_cache_control` ?



#### Rails assets group

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


### Rails and databases

* Do not store database.yml in version control
* Since Rails 4.1 rails can load the DB from the `DATABASE_URL` environment
* variable (provided it is set to a valid connection string)

QUESTION: what does a valid connection string look like?

QUESTION: what is the correct database pool size for rails? - read the guide

### Rails secrets.yml

* `config/secrets.yml`
    * You can put API secret keys etc. in here
    * Rails requires a `secret_key_base` be set for each environment.
        * In Rails 4.0 it was set in `config/secret_token.rb`
        * In Rails 4.1+ it is in `secrets.yml`

They strongly recommend keeping all app secrets as environment vars

```yaml
# config/secrets.yml
production:
  secret_key_base: <%= ENV['SECRET_KEY_BASE'] %>
```

Aside: You can run ruby in Yaml files because Erb is built-in to ruby

Rails makes the contents of secrets.yml available in `Rails.application.secrets`

```
[1] pry(main)> Rails.env
"development"
[2] pry(main)> Rails.application.secrets
{
    :secret_key_base => "secret stuff ..."
}
[4] pry(main)> Rails.application.secrets.secret_key_base
"secret stuff ..."
```

### Logging

* Most Rails objects have a `logger` attribute
* `logger` conforms to the `Log4r` (default ruby 1.8+ logger) interface

```ruby
# The Log4r interface (in increasing severity)
logger.debug  # 1
logger.info   # 2
logger.warn   # 3
logger.error  # 4
logger.fatal  # 5
```

* even if `logger` isn't available you can use `Rails.logger` which is a global
  instance of a logger

* use `rake log:clear` to truncate the rails log files
* the log outputs shows SQL querys executed and hits to the Rails SQL query cache

You can add any arbitrary request object data to the log entry by setting the
`config.log_tags` setting in

```ruby
# any property of request can be used here
config.log_tags = [:subdomain, :original_url] # takes an array
```

This lets you group log entries together to diagnose problems

TODO: What are the properties available on the rquest object

* times reported by the logger are not super accurate but are good enough to be
  compared with each other and see trends over time

#### N+1 query problem

* An N+1 problem happens when you are displaying a record along with an associated
  collection e.g. showing a list of blog posts and the full details of one in a
  master-detail  view.
* You can recognise the problem by a series of `SELECT` statements that are
  different only in the primary key
* DB problems are worse when your DB is on a different machine to your app as
  each query has the latency problem to deal with too

#### log tips

* You can replace the default logger objects with your own so you customise
  logging as much as you want
* You can even do this in the console on a live running system
* The logger objects to replace are
    * `ActiveRecord::Base.logger`
    * ???
    * TODO: more at  http://weblog.jamisbuck.org/2007/1/31/more-on-watching-activerecord.
* You can also send Rails logs to syslog with `SyslogLogger` gem.
    * syslog can consolidate logs from multiple apps, can log remotely etc.
* use `less -R` to view colorized output in pager
* `tail` already supports colorized output
