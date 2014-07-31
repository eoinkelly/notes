#Summary of Rails 2 Routing

```ruby
# create a single named route with
map.connect '/login', :controller => 'sessions', :action => 'new

# same as above but also create path helpers:
#   * logout_path
#   * logout_url
map.login '/login', :controller => 'sessions', :action => 'new

# constrain to a HTTP method by adding a param
map.connect 'photo/:id', :controller => 'photos', :action => 'show', :conditions => { :method => :get }

# create a set of restful routes
map.resources :books
```

# default catch-all routes
```ruby
map.connect ':controller/:action/:id'
map.connect ':controller/:action/:id.:format'
```

* Catch-all are not recommended in Rails 3+


* :foo (anything that looks like a symbol) within a route is a `segment key` and
  will match that segment in the URL
* `:controller/:action/:id` will match the first section to a controller, the next
  to an action and so on.
* Rails routing 2.x does not mention query parameters - rails will put them in
  the params hash but they are not part of the routing system

# Globbing routes

You can glob routes to match the rest of the URL:

```ruby
map.connect 'photo/*other' ...
```
