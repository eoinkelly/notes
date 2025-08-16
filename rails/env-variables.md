## Environment variables

- RAILS_ENV
    - Usually one of production|development|test

- BUNDLE_GEMFILE
    - set by `config/boot.rb` to point to the project's Gemfile
    - QUESTION: who uses it?

## Rails object

- Rails.application
    - seems to be the rails application
- Rails.groups
    - an array of the Gemfile groups ???
- Rails.env
    - an object representing the rails env
    - `Rails.env.development? # bool`
        - it presumably uses some method_missing magic to make this work
    - `Rail.env # => string rep of the env name`
