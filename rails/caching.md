# Rails caching

Base API

```ruby
Rails.cache.fetch("somekey") # getter
Rails.cache.fetch("somekey") do # setter
    # result of block is written to the given key
    { some: :value }
end
```

* cache keys can be arrays or hashes (or anything that responds to `to_param`, `cache_key`)

## tips

* to see the state of caching in a project:
    1. see if it is on/off in `config/environments/*.rb`
    2. grep the codebase for `caches_` and `cache_` - the relevant functions are
        * `cache_page`
        * `caches_page`
        * `cache_action`


## About caching

* all caching is turned on/off by the `config.action_controller.perform_caching` variable in `config/environments/*.rb`
* caching is disabled by default in development and test, enabled in production



## 3 types of caching available

1. page caching
    * in `actionpack-page_caching` gem since rails 4
    * https://github.com/rails/actionpack-page_caching
    * how it works
        * the entire action output is stored as a HTML file on filesystem
    * how to set it up
        1. you set a directory for the files to be stored using
          `config.action_controller.page_cache_directory`
        2. use `caches_page :show, :index` method in the controller to tell rails
          which actions to cache
        3. configure apache/nginx to check the cached pages dir first and only
          pass the request to the rails app if the pages are not found
    * expiration:
        * expire pages by simply deleting the generated files
        * done using the `expire_page` method in the controller
        * can be done in any action which you know will change content e.g.
          `update`
        * can also use sweepers from the `rails-observers` gem to trigger
          `expire_page` which models change
    * cannot bue used if:
        * page has any state that changes between requests e.g. authenticated user
    * good for:
        * CMS pages that rarely/never change
    * pros/cons
        * ++ very fast caching as rails is not involved at all
        * -- expiration is very manual
        * -- only suitable for pages with totally static content
        * -- not suitable for actions with any before filters e.g. auth
    * aside: `cache_page 'foo'` can be used to manually cache a string - it is called by `caches_page`
2. action caching
    * in `actionpack-action_caching` gem since rails 4
    * https://github.com/rails/actionpack-action_caching
    * how it works
        * the incoming request hits the rails stack so before filters can be run on it before the cached output is served
        * the entire output of the response is cached i.e. the whole HTML page
        * uses _fragement caching_ and an around filter to do its job
        * the cached fragment is named after the host of the requester (so each requester gets a different cached fragment
        * it does not serve the same content to eveybody but if I log in and repeatedly ask for the same content it will use the cache for repeated requests of the same action
    * how to set it up
        1. use the `caches_action` method in the controler to say which actions to cache
        1. choose options like
            * `:exprires_at` to schedule expiration
            * `layout: false` to not cache the layout
            * add an optional Proc to calculate when expiration should happen
3. fragment caching
    * ++ allows for more granular caching than a whole action or whole page
    * allows a fragment of a view to be wrapped in a cache block and be served out of the cache next time
    * keys can be
        1. ActiveRecord model (under the hood calls `#cache_key` on the model which returns a string key based on model name, model id, updated_at timestamp.
        2. (controller, action, action_suffix) tuple
        3. a string (global key)
        4. a string returned by evaluating some helper
    * `cache_if(condition, key)` and `cache_unless(condition, key)` are also available

```haml
-# bind to the action that called it and write the cached content to the same place as the action cache
- cache do
    - @products.each do |p|
      = p.name

-# name the fragment uniquely
- cache(action: 'index', action_suffix: 'all_products) do
    - @products.each do |p|
      = p.name

-# use a global key to make this fragment accessible to all views
- cache('all_our_products') do
    - @products.each do |p|
      = p.name

-# nested caching strategies ("russian doll" caching)
-# if the outer fragment is regenerated, then the inner fragments will be too
- cache('all_our_products') do
  %h1 All products
  - Product.all.each do |p|
    - cache(p) do
      = p.name
```

* you can expire cached fragments by refering to them via their key
```ruby
# key is controller, action, action_suffix tuple
expire_fragment(controller: 'products', action: 'index', action_suffix: 'all_products')

# key is global
expire_fragment('all_our_products')
```

    * You can have multiple cached fragments per action so you need to name them

## Caching things other than view fragments

You can store _any_ value in the cache

* Use `cache` to store
* Use `Rails.cache.fetch` to retrieve

```ruby
class Product < ActiveRecord::Base
  def competing_price
    Rails.cache.fetch("#{cache_key}/competing_price", expires_in: 12.hours) do
      Competitor::API.find_price(id)
    end
  end
end
```

QUESTION: what are the implications of this for avoiding work in a rails app?

## Cache storage

* `config.cache_store` variable in `config/environments/*.rb` controls this
* ??? what storage options available
* ??? which types can be used

# Rails low level cache API

Rails.cache.read/write/fetch

## Caching on heroku

* https://devcenter.heroku.com/articles/caching-strategies
* heroku recommends against "automatic" cache gems like `cache-money` or `cache_fu`
* full page caching will not work on heroku as their file store is ephermeral.

Caching options:

1. Use action caching
    * requires a memcache addon (e.g. memcached, redis)
2. Use fragment caching
    * requires a memcache addon (e.g. memcached, redis)
3. Use Rack::cache as a reverse proxy and avoid requests to the app entirely
    * todo  find out more abou this

There are many redis addons for heroku - "rediscloud" seems to have the best free tier
`
