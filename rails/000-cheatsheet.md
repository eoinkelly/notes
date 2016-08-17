# Create new rails app

```
rails new APP_NAME --skip-turbolinks --skip-test-unit --database=postgresql --skip-spring
cd APP_NAME

# create dotfiles
.scss-lint.yml
.ruby-version
.rubocop.yml
.eslintrc.yml
.env

## edit Gemfile ####################
ruby "2.3.1"

gem "unicorn" # if required

group :development, :test do
  gem "pry-rails"
  gem "pry-byebug"
  gem "rspec-rails"
  gem "rubocop", require: false # CI needs rubocop in the bundle
  gem "scss_lint", require: false
  gem "dotenv-rails" # loads .env file
  gem "factory_girl_rails"
end
#####################################

#####################
# add to .gitignore
/.env
/.envrc
#####################


Copy in a scripts/ dir

# setup rspec
rails g rspec:install
bundle binstubs rspec-core
rspec spec # to check all is well


# rename app/assets/stylesheets/application.css to .scss
# make it an empty file


# rename readme.rdoc to .md - remove filler content
```
