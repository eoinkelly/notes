

```ruby
page.class # => Capybara::Session
# these are the same, default maximum wait is 2 secs, can be changed
page.find(".foo")
page.find(".foo", wait 2)

# must have element as first arg
page.find("div", text: "blah")

# find the first .blah on the page which is hidden
page.find(".blah", visible: false)

page.find("h1", text: "Some content")

# these are the same
page.find("h1", exact_text: "Some content")
page.find("h1", text: "Some content", exact_text: true)
```
