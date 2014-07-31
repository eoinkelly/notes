# Rails Routing

* how the 7 actions map to CRUD

presentation centric:
1. index -> R
2. show -> R
3. new -> R
persistence centric:
4. create -> C
5. edit -> U
6. update -> U
7. destroy -> D

* When you use `resources :foos` _every_ route gets a path helper created for it
* Routing matches the order given in the routes file (put wildcards lower down so they don't match too early)
* The routing DSL splits expects the value (RHS) of the hash you pass to its methods (#get, #resources etc.) to be a string of the form "controllername#actionname"

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
figure out routing concerns, namespaces
