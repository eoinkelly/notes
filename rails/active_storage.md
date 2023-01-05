```ruby
# set app default to proxy
Rails.application.config.active_storage.resolve_model_to_route = :rails_storage_proxy

# both these change by whether the app default is to proxy or redirect
url_for(some_image_ob)
rails_blob_(path|url)(some_image_ob, disposition: :inline|:attachment) # same as above but lets you set disposition

# explicitly ask for the proxy url
rails_storage_proxy_path(@user.avatar)
rails_storage_proxy_url(@user.avatar)

# from a model or other ruby class (notice the only_path)[
Rails.application.routes.url_helpers.rails_blob_path(user.avatar, only_path: true)


# disable drawing of all ActiveStorage routes
config.active_storage.draw_routes = false


# this will generate a req to the AS redirect controller which will then rediret to the actual file on storage
file.representation(resize_to_limit: [100, 100]).url

# add .processed to generate the URL for the image immediately
file.representation(resize_to_limit: [100, 100]).url
```


the app uses
  url_for()
  url_for(img.variant())

in tests:
              url_for(main_image.variant(resize_to_limit: [336, 225])).remove('http://www.example.com')
