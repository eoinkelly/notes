url_for

- passing a record (e.g. an ActiveRecord object)

```
app.url_for
app.url_options
app._routes.url_for
app.polymorphic_url
```

- if `current_url` is defined then `url_for` can be called with just those
  params you wish to change
