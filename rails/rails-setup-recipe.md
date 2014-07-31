# Eoin's Rails Recipe

* Make sure postgres server is up and running

Q: how does rails know what username + pass to use for psotgres?
A: If the database role is blank, postgres will use the operating system user and ??? password
    * TODO: research a bi tmore into how postgres does permissions

```bash
gem update rails
rails new --skip-test-unit --database=postgresql app-name
cd app-name
git init
git add -A
git commit -m "Initial commit"
```

* Add the following to `Gemfile`

```ruby
# ./Gemfile
group :development, :test do
  gem 'rspec-rails', '~> 3.0.0'
end
```

```bash
bundle install
spring stop # spring seems to break `rails g rspec:install` ???
rails g rspec:install
bundle binstubs rspec-core
git add -A
git commit -m "Setup Rspec"
```

todo
remove turbolinks
