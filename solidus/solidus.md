# Solidus

## Questions

- it seems to load the mysql and sqlite gems as well as pg in
  common_spree_depenendencies.rb can we stop that? Does an engine's gemfile get
  loaded when it is mounted? if not the ignore the above

## Intro

Solidus is made up of 5 gems

1. solidus_api (RESTful API)
2. solidus_frontend (Cart and storefront)
3. solidus_backend (Admin area)
4. solidus_core (Essential models, mailers, and classes)
5. solidus_sample (Sample data)

The 'solidus_auth_devise' is also used somehow???

solidus is designed to be run as an rails engine

what does it use for authorization?

## Solidus sample

- a gem
- a sample app

## Deface

- uses CSS style selectors to target overrides of parts of erb templates
- https://guides.spreecommerce.com/developer/view.html

# My thoughts on it

- deface seems like a reasonable tool for inserting targetted overrides
    - ??? does it make figuring out where your view code is comeing from very
      complex?
        - how does one figure that out?

- you can override a whole view by just including the erb file in the correct
  place
- you can override bits of a view using deface

## Backend

gem depends on

    s.add_dependency 'activemerchant', '~> 1.48.0'
    s.add_dependency 'acts_as_list', '~> 0.3'
    s.add_dependency 'awesome_nested_set', '~> 3.0.1'
    s.add_dependency 'carmen', '~> 1.0.0'
    s.add_dependency 'cancancan', '~> 1.10'
    s.add_dependency 'ffaker', '~> 1.16'
    s.add_dependency 'font-awesome-rails', '~> 4.0'
    s.add_dependency 'friendly_id', '~> 5.0'
    s.add_dependency 'highline', '~> 1.6.18' # Necessary for the install generator
    s.add_dependency 'json', '~> 1.7'
    s.add_dependency 'kaminari', '~> 0.15', '>= 0.15.1'
    s.add_dependency 'monetize', '~> 1.1'
    s.add_dependency 'paperclip', '~> 4.2.0'
    s.add_dependency 'paranoia', '~> 2.1.4'
    s.add_dependency 'premailer-rails'
    s.add_dependency 'rails', '~> 4.2.0'
    s.add_dependency 'ransack', '~> 1.6.0'
    s.add_dependency 'responders'
    s.add_dependency 'state_machines-activerecord', '~> 0.2'
    s.add_dependency 'stringex', '~> 1.5.1'
    s.add_dependency 'truncate_html', '0.9.2'
    s.add_dependency 'twitter_cldr', '~> 3.0'
    s.add_dependency 'sprockets-rails', '~> 2.0'

## Core

Gem depends on

    s.add_dependency 'activemerchant', '~> 1.48.0'
    s.add_dependency 'acts_as_list', '~> 0.3'
    s.add_dependency 'awesome_nested_set', '~> 3.0.1'
    s.add_dependency 'carmen', '~> 1.0.0'
    s.add_dependency 'cancancan', '~> 1.10'
    s.add_dependency 'ffaker', '~> 1.16'
    s.add_dependency 'font-awesome-rails', '~> 4.0'
    s.add_dependency 'friendly_id', '~> 5.0'
    s.add_dependency 'highline', '~> 1.6.18' # Necessary for the install generator
    s.add_dependency 'json', '~> 1.7'
    s.add_dependency 'kaminari', '~> 0.15', '>= 0.15.1'
    s.add_dependency 'monetize', '~> 1.1'
    s.add_dependency 'paperclip', '~> 4.2.0'
    s.add_dependency 'paranoia', '~> 2.1.4'
    s.add_dependency 'premailer-rails'
    s.add_dependency 'rails', '~> 4.2.0'
    s.add_dependency 'ransack', '~> 1.6.0'
    s.add_dependency 'responders'
    s.add_dependency 'state_machines-activerecord', '~> 0.2'
    s.add_dependency 'stringex', '~> 1.5.1'
    s.add_dependency 'truncate_html', '0.9.2'
    s.add_dependency 'twitter_cldr', '~> 3.0'
    s.add_dependency 'sprockets-rails', '~> 2.0'

## API

    gem.add_dependency 'solidus_core', gem.version
    gem.add_dependency 'rabl', ['>= 0.9.4.pre1', '< 0.12.0']
    gem.add_dependency 'versioncake', '~> 2.3.1'

## About the dependencies

- carmen
    - Carmen- A repository of geographic regions for Ruby
    - used to populate Spree::Country, Spree:State tables in the DB
- friendly_id
    - https://github.com/norman/friendly_id
    - It allows you to create pretty URL’s and work with human-friendly strings
      as if they were numeric ids for ActiveRecord models
    - FriendlyId is an add-on to Ruby's Active Record that allows you to replace
      ids in your URLs with strings:
    - To the extent possible, FriendlyId lets you treat text-based identifiers
      like normal IDs. This means that you can perform finds with slugs just
      like you do with numeric ids:
    - http://norman.github.io/friendly_id/file.Guide.html
- rabl
    - https://github.com/nesquena/rabl
    - RABL (Ruby API Builder Language) is a Rails and Padrino ruby templating
      system for generating JSON, XML, MessagePack, PList and BSON.
- cancancan
    - used for authorization

## General notes

- it loads seeds using the `db:load_dir` task which has to have `reenable`
  called on it first
- it has the idea of "friendly id" for models that you can use
- uses the http://getskeleton.com/ CSS framework
- most of the models seem to be in core gem
