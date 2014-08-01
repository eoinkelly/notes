# Rails Routing

Has 2 purposes

1. map requests to controller actions
2. create methods for dynamic URL generation
    * these all return a string representing a URL or path


## routes.rb

`config/routes.rb` contains a list of _rules_ called routes.
* eash route is a method call that takes a hash
* the first _key: value_ in the hash has this form:
    * The key 
        * is made up of 3 types of thing:
          1. `/` matches literal `/` in the URL
          2. segment keys e.g. `:id`
          3. static substrings
        * provides the pattern for
          1. matching the URL (recognition)
          2. creating the URL in the helper (generation)
    * The value 
        * is usually a string of the form `controller#action`
        * is a rack endpoint.
    * Q ??? can the value contain segment keys? I thik so?

* The URLs crated by the helpers only make sense to the routing system that
  created them - they are not necessairly human understandable.

The format of a route is a method call:

```ruby
get 'recipes/:ingredient' => 'recipes#index'
get({'recipes/:ingredient' => 'recipes#index'})
# {method-name} {params-hash}
# get
# post
# patch
# push
# put
# delete
# match
```

* The methods declare which HTTP verb this route should match (or any verb if it
  is `match`)

Routes have a rich syntax. A single route has to provide enough info to match an
existing URL and create a new one (with the helper method)


All routes are defined inside a block passed to the `Rails.application.routes.draw` method.

Q: can you add routes to rails from outside routes.rb? Probably a bad idea but ...

```sh
[3] pry(main)> Rails.application.routes.class
ActionDispatch::Routing::RouteSet < Object
[4] pry(main)> Rails.application.routes.methods(false)
[]
```



```ruby
Rails.application.routes.draw do
  get 'products/:id', to: 'products#show' # old syntax
  get 'products/:id => 'products#show'    # same as above

  match 'products/:id => 'products#show', via: :get
  get   'products/:id => 'products#show'              # same as above

  # the old syntax can be still useful
  match 'products/:id => 'products#show', via: [:get, :post] 

  # raises exception because no method specified
  match 'products/:id => 'products#show'

  # explicitly allow all methods (probably a dumb idea)
  match 'products/:id => 'products#show', via: :any

  # You can hard code arbitrary key values into `params` in the route
  # NOTE: you should probably hit a different route than do this
  match 'products/:id => 'products#show', via: :any, special: true
end
```

The routing block is evaluated inside `ActionDispatch::Routing::Mapper` class at
runtime.

## Segment keys

* `:foo` in a route pattern
* optional segment keys are wrapped in parens `/products/:id(.:format)/`

## link_to

The raw form of `link_to` provides values for all the segment keys in the route.
* segment keys are passed to the controller in `params` 

```ruby
# this does not work for a route that doesn't exist
link_to "Some stuff", controller: "products", action: "show", id: 1
```

Q: can I use link_to to make a route that doesn't exist in routes.rb?

* `link_to` does not have any special understanding of what routes exist - it is
  just a pure function that takes some args and builds a string.


## Redirect routes

```
match '/foo', to: redirect('/bar') # DOES NOT WORK, warns about missing HTTP method constraint

get '/foo', to: redirect('/bar')                    # relative URL
get '/foo' => redirect('/bar')                      # alternate syntax
get '/foo', to: redirect('/bar', status: 302)       # optional HTTP status
get '/foo', to: redirect('http://www.foo.com/bar')  # absolute URL

# redirect can take a block to do a complex redirect
match "/api/v1/:api",
      to: redirect { |params| 
        "/api/v33/#{params[:api].pluralize}"
      },
      via: :any

# same as above with optional status param
match "/api/v1/:api",
      to: redirect(status: 301) { |params| 
        "/api/v33/#{params[:api].pluralize}"
      },
      via: :any
```

* `redriect` method returns `ActionDispatch::Routing::Redirect` which is a
  _simple Rack endpoint_.

Q: what is a simple rack endpoint?
    I think it is any thing that returns `[status, headers, [body]]`

## The :format field

* segment named `:format` has a special meaning to the `respond_to` method of
  the controller.
* `respond_to` checks `:format` to decide which of the provided blocks to run in
  your controller.
* Rails will throw an exception if you specify a format that isn't covered by
  `respond_to` in the controller

```ruby
respond_to do |format|
  format.html { do stuff }
  format.json { stuff.to_json }
  format.any { # catch all }
end
```

## How router and controllers are coupled

TL;DR The router runs rack endpoints. Controllers have #action which wraps
their methods as an endpoint.

* the argument to `to:` is a rack endpoint.
* Rails controllers have an #action method that returns a _rack endpoint_ that
  executes the specified action i.e.
* The `controller#actionname` syntax just runs this #action method.
* so the router and controllers are loosely coupled.

```ruby
xx = UsersController.action(:show)
# xx is a rack endpoint that when invoked will run UsersController#show
```

So the rails router can route to _any_ rack endpoint.

```ruby
# make a simple rack endpoint inline in the router!
get '/hithere', to: proc { |env| 
  # [status, headers, [body]]
  [200, {}, ["I am the body hi there"]]
}
```

## Mounting another rack app in your rails app

Because the router just runs rack endpoints you can run _any_ rack endpoint from
your rails app e.g. a Sinatra app

```ruby
mount SinatraAppMainClass, at: '/some-path'
mount SinatraAppMainClass => '/some-path' # same as above
```


up to 2.2.10


## How the 7 actions map to CRUD

* presentation centric:
    1. index -> R
    2. show -> R
    3. new -> R
* persistence centric:
    4. create -> C
    5. edit -> U
    6. update -> U
    7. destroy -> D

* When you use `resources :foos` _every_ route gets a path helper created for it
* Routing matches the order given in the routes file (put wildcards lower down
  so they don't match too early)
    * Rails stops looking for thorugh the routes when it finds the first match.


```ruby
# Implment exactly what resources :decks would do
get    "/decks"          => "decks#index"
post   "/decks"          => "decks#create"
get    "/decks/new"      => "decks#new",  as: :new_deck
get    "/decks/:id/edit" => "decks#edit", as: :edit_deck
get    "/decks/:id"      => "decks#show", as: :deck
patch  "/decks/:id"      => "decks#update"
put    "/decks/:id"      => "decks#update"
delete "/decks/:id"      => "decks#destroy"

#alternatively this does exactly the same
resources :decks
```

`resources :plural` creates 7 routes and 4 path helpers named as follow

1. new_singular_(path|url)
2. edit_singular_(path|url)
3. singular_(path|url)
4. plural_(path|url)

### Setting the base URL for *_url helpers

* By default it is taken from the request (??? check this)
* You can tweak it by defining #default_url_options in your controllers
    * #default_url_options returns a hash of options, not a string
    * Put it in ApplicationController to tweak it everywhere or in a specific controller to just tweak it in those views


### The many types of args that path helpers can take

```ruby
# Multiple ways to call path helpers (with or without hash as param)
app.deck_path(id: 1)
app.deck_path(1)
app.deck_url(id: 1)
app.deck_url(1)

x = Deck.find(1) # x is anything that will respond to #id
app.deck_path(x)
```

### #polymorphic_path

Rails controller and view methods like:

* form_for
* link_to
* redirect_to

* all internally use `#polymorphic_path` under the hood (you can get at it via `app.polymorphic_path` in rails console).
* It takes almost anything railsey and figures out what the path to it would be.

app.polymorphic_path(x, options)

*  x could be
    * A model class name e.g. `User`
    * An instance of an active_record model `User.find(1)`
    * An array of models or classes or ids (for nested resources)
* it expects its second arg to be an options hash so if you have nested routes you need to pass them in an array

```ruby
# all these do the same because #link_to uses #polymorphic_path under the hood
= link_to 'A deck', deck_path(@deck.id)
= link_to 'A deck', deck_path(@deck) # polymorphic_path will call #id

# #polymorphic_path will inspect the type and also call #id
# notice we are not using a path helper method here, we are instead passing an arg that will be given directly to polymorphic path
= link_to 'A deck', @deck

# we can't remove the *_path method here as polymorphic path will give you back the URL for displaying what you give it, not editing it
= link_to 'Edit a deck', edit_deck_path(@deck)
```

```ruby
app.polymorphic_path(User) # => "/users"
app.polymorphic_path(User.find(3)) # => "/users/3"
app.polymorphic_path([Section.find(4), User.find(3)]) # => "sections/4/users/3"


# works because rails knows what models to use from method name
# Note: args _not_ passed as array
app.deck_card_path(4,1)

# fails because rails doesn't know what models to use
# Note args are passed as array
app.polymorphic_path([4, 1])

# works because rails knows it can call #id on them
app.polymorphic_path([@deck, @card])

```

# Nesting resources

```ruby
resources :foos do
  resources :bars
end
```
* creates the normal 7 foo resources
* creates the normal 7 bar resources except for 2 differences:
    1. the bar _paths_ are nested within a single foo path `foos/:id`
    2. the path helpers are named with `foo_bar` not with `bar`
* the foo resources are placed *after* the bar resources so that a bar will match first

```
      Prefix Verb   URI Pattern                           Controller#Action
    foo_bars GET    /foos/:foo_id/bars(.:format)          bars#index
             POST   /foos/:foo_id/bars(.:format)          bars#create
 new_foo_bar GET    /foos/:foo_id/bars/new(.:format)      bars#new
edit_foo_bar GET    /foos/:foo_id/bars/:id/edit(.:format) bars#edit
     foo_bar GET    /foos/:foo_id/bars/:id(.:format)      bars#show
             PATCH  /foos/:foo_id/bars/:id(.:format)      bars#update
             PUT    /foos/:foo_id/bars/:id(.:format)      bars#update
             DELETE /foos/:foo_id/bars/:id(.:format)      bars#destroy
        foos GET    /foos(.:format)                       foos#index
             POST   /foos(.:format)                       foos#create
     new_foo GET    /foos/new(.:format)                   foos#new
    edit_foo GET    /foos/:id/edit(.:format)              foos#edit
         foo GET    /foos/:id(.:format)                   foos#show
             PATCH  /foos/:id(.:format)                   foos#update
             PUT    /foos/:id(.:format)                   foos#update
             DELETE /foos/:id(.:format)                   foos#destroy
```

## The core methods of the rails routing DSL

* #resource
* #resources
* #get
* #put
* #patch
* #delete
* #post
* #root
* #concern
* #namespace

# TODO
what does resource (singluar) do?
figure out routing concerns, namespaces, collection



