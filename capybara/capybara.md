# Capybara

## Sources

* Capybara docs
* http://robots.thoughtbot.com/write-reliable-asynchronous-integration-tests-with-capybara

## About

Capybara is a translator layer

```
test framework layer    [rspec, cucumber                                  ]
capybara layer          [capybara                                         ]
driver layer            [Rack::Test    ] [Selenium webdriver] [poltergeist]
                        [rails][sinatra] [real browsers     ] [phantomjs  ]
```

Capybara has 2 main parts

* DSL for creating tests
* Backends e.g.
    * Selenium webdriver
    * Rack
    * Poltergeist

## Classes of interest

* `Capybara::Window`
    * represents a browser window
* `Capybara::Session`
    * represents a single user's interaction with the system
    * can use any available driver
    * imporant methods
        * `#visit`
        * `#current_path`
    * delegates many methods to `Capybara::Document` so essentially you can
      think of a Session as having a superset of the Document|Element API
    * `page` in your test is an instance of `Capybara::Session`
* `Capybara::Document`
    * represents the HTML document
* `Capybara::Result`
    * A Result represents a collection of `Capybara::Node::Element` on the page. It
      is possible to interact with this collection similar to an `Array`
      because it implements Enumerable and offers Array methods through delegation
* `Capybara::Node::Base`
* `Capybara::Node::Element`
* `Capybara::Query`
    * Does the bulk of the searching work when you call a finder
        * `#resolve_for` returns the result of the query

Element and Document are both children of Node::Base. They share 3 kinds of methods

1. Finders
    * examples
        1. `#all`
            * returns Capybara::Result
        2. `#find`
            * returns Capybara::Node::Element
        3. `#find_button`
            * returns Capybara::Node::Element
            * wrapper around #find
        4. `#find_by_id`
            * returns Capybara::Node::Element
            * wrapper around #find
        5. `#find_field`
            * returns Capybara::Node::Element
            * wrapper around #find
        6. `#find_link`
            * returns Capybara::Node::Element
            * wrapper around #find
        7. `#first`
            * returns Capybara::Node::Element
            * wrapper around #all
    * most of these methods return instances of Node::Element (< Node::Base) so you call any of these
      methods on what you get back from a Finder
    * the exception is #all which returns a Capybara::Result which is an enumerable collection
    * The `find_*` methods are all simple wrappers for `find`
    * `find` based methods will wait for `default_wait_time` seconds if the driver supports JS
    * `first` is a wrapper for `all`
2. Actions
    * all these "do stuff" to the the selected Node::Base instance
    * QUESTION: what do they return? self?
    * examples
        1. `#attach_file`
        2. `#check`
        3. `#choose`
        4. `#click_button`
        5. `#click_link,`
        6. `#click_link_or_button`
        7. `#fill_in`
        8. `#select`
        9. `#uncheck`
        10.`` #unselect
    * Internally these use `#find` to locate what they need and then maniuplate it.
        * => They inherit `find` wait behaviour
3. Matchers
    * these methods fall into 2 categories:
        1. asserts
            * return true if pass
            * return Capybara::ExpectationNotMet if fail
        2. predicate methods
            * return true|false
    * examples
        1. `#assert_no_selector`
        2. `#assert_no_text`
        3. `#assert_selector`
        4. `#assert_text,`

        5. `#has_button?`
        6. `#has_checked_field?`
        7. `#has_css?`
        8. `#has_field?`
        9. `#has_link?,`
        10. `#has_no_button?`
        11. `#has_no_checked_field?`
        12. `#has_no_css?`
        13. `#has_no_field?,`
        14. `#has_no_link?`
        15. `#has_no_select?`
        16. `#has_no_selector?`
        17. `#has_no_table?,`
        18. `#has_no_text?`
        19. `#has_no_unchecked_field?`
        20. `#has_no_xpath?`
        21. `#has_select?,`
        22. `#has_selector?`
        23. `#has_table?`
        24. `#has_text?`
        25. `#has_unchecked_field?`
        26. `#has_xpath?`
4. Node methods
    * These are stubbed out in capybara but must be implemented by whatever driver you use
    * It seems like drivers subclass Driver::Node
    * The "action" methods above use #find to get the Node::Element and then manipulate it with these
    * => they have a different implementation in different drivers
    * Examples:
        1. `==(other)`
        2. `[](name)`
        3. `all_text`
        4. `checked?`
        5. `click`
        6. `disabled?`
        7. `double_click`
        8. `drag_to(element)`
        9. `hover`
       10. `initialize(driver, native)` constructor
       11. `inspect`
       12. `path`
       13. `right_click`
       14. `select_option`
       15. `selected?`
       16. `set(value, options = {})`
       17. `tag_name`
       18. `trigger(event)`
       19. `unselect_option`
       20. `value`
       21. `visible?`
       22. `visible_text`



## Drivers

Inheritance Heirarchy

```
Object
    Capybara::Driver::Base
        RackTest::Driver
        Selenium::Driver
    Capybara::Driver::Node
        RackTest::Node
        Selenium::Node
```

* Driver::Base methods:
    ```
    accept_modal
    browser_initialized?
    close_window
    current_url
    current_window_handle
    dismiss_modal
    evaluate_script
    execute_script
    find_css
    find_xpath
    go_back
    go_forward
    html
    invalid_element_errors
    maximize_window
    needs_server?
    no_such_window_error
    open_new_window
    reset!
    resize_window_to
    response_headers
    save_screenshot
    status_code
    switch_to_window
    visit
    wait?
    window_handles
    window_size
    within_frame
    within_window
    ```
* Driver::Node methods
    ```
    =
    []
    all_text
    checked?
    click
    disabled?
    double_click
    drag_to
    hover
    initialize
    inspect
    path
    right_click
    select_option
    selected?
    set
    tag_name
    trigger
    unselect_option
    value
    visible?
    visible_text
    ```



You can get at the current driver (which will have real implementations of these methods) via `page.driver`


# Getting stuff from a Capybara::Node::Element

If you have an instance of Capybara::Node::Element, how do you find out stuff about it

```ruby
page.class # Capybara::Session
page.document.class # Capybara::Document
page.document.session == page # true

page.find('body').class # Capybara::Node::Element < Capybara::Node::Base
```

```ruby
aa = page.find('a') # aa is a Capybara::Node::Element

aa.value    # => nil
aa.tag_name # => 'a'
aa.text     # => "text content of the <a> tag
aa[:href]   # => "/foo/bar" (notice the [] gets attributes (& properties ???) of the node
```

* use [] to access attributes

## locator

THe concept of a "locator" string is important in Capybara
The locator is one of
* a jQuery style css selector to traverse the DOM
* an XPath expression

# Handling async content with Rspec

* To interact with page prefer _action methods_ over _finder methods_
* To verify contents of page, prefer _RSpec matchers_ over _node matchers_
* @joeferris uses `find('.thing')` as an _intellegent sleep_ to wait for the thing to be
  on the page before doing something else with it.
* `Capybara.default_wait_time` = amount of time capybara will wait
* capybara only waits if the driver is capable of running javascript

Question: his blog post indicates that #first and #all do not wait


Some good/bad ways of getting stuff on the page that is loaded asynchronously by JS

```ruby
# If you want to make sure there's exactly one
find(".active").click

# If you just want the first element
find(".active", match: :first).click


# Bad:
all(".active").each(&:click)

# Good:
find(".active", match: :first) # waits
all(".active").each(&:click)


# Bad
execute_script("$('.active').focus()")

# Good (find will wait for .active to be available)
find(".active")
execute_script("$('.active').focus()")


#NOTE: the following show that have_* methods are friendlier to async content

# Bad:
expect(find_field("Username").value).to eq("Joe")
# Capybara will wait for the matching element and then immediately return its
# value. If the value changes from a page load or Ajax request, it will be too
# late.

# Good:
expect(page).to have_field("Username", with: "Joe")
# Capybara will wait for a matching element and then wait until its value
# matches, up to two seconds.


# Bad:
expect(has_css?(".active")).to be_false
# Capybara will immediately return true if the element hasn't been removed from
# the page yet, causing the test to fail. It will also wait two seconds before
# returning false, meaning the test will be slow when it passes.

# Good:
expect(page).not_to have_css(".active")
```

## Matchers that it adds to RSpec

* These all express expectations about the state of the `page` object (instance of Capybara::Session)
* They are smart enough to wait up to `default_wait_time` if the don't succeed the first time they try.
* The are all convenience constructors for `HaveSelector.new(...stuff...)`
* They are handy in cases where a particular element is on the page but its contents are loaded aysnchronously

- become_closed(options = {})
- have_button(locator, options = {})
- have_checked_field(locator, options = {})
- have_css(css, options = {})
- have_field(locator, options = {})
- have_link(locator, options = {})
- have_select(locator, options = {})
- have_selector(*args)
- have_table(locator, options = {})
- have_text(*args) (also: #have_content)
- have_title(title, options = {})
- have_unchecked_field(locator, options = {})
- have_xpath(xpath, options = {})

# random notes

find(query_type, query_ptions)

query_type = :css (default) or :xpath

A common pattern for

* fill_in
* select
* check

etc. is that their locators are

1. name
2. id (without #)
3. label


fill_in does use find() under the hood so it will magically wait
