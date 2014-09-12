# Capybara

TODO: summarise exactly which Capybara methods are NOT wait friendly?

## Sources

* http://robots.thoughtbot.com/write-reliable-asynchronous-integration-tests-with-capybara


# Summary of skillful Capybara usage

* To interact with page prefer _action methods_ over _finder methods_
* To verify contents of page, prefer _RSpec matchers_ over _node matchers_
* @joeferris uses `find('.thing')` as an _intellegent sleep_ to wait for the thing to be
  on the page before doing something else with it.
* `Capybara.default_wait_time` = amount of time capybara will wait
* capybara only waits if the driver is capable of running javascript

## X Capybara DSL methods

???
TODO

## 10 Node Actions

* These have a pattern of using `#find` to locate what they need and then maniuplating it.
    * => They inherit `find` wait behaviour

- (Object) attach_file(locator, path, options = {})
- (Object) check(locator, options = {})
- (Object) choose(locator, options = {})
- (Object) click_button(locator, options = {})
- (Object) click_link(locator, options = {})
- (Object) click_link_or_button(locator, options = {}) (also: #click_on)
- (Object) fill_in(locator, options = {})
- (Object) select(value, options = {})
- (Object) uncheck(locator, options = {})
- (Object) unselect(value, options = {})


## 7 Finder methods

- (Capybara::Result) all([kind], locator, options)
- (Capybara::Element) find(*args)
- (Capybara::Element) first([kind], locator, options)
- (Capybara::Element) find_button(locator, options = {})
- (Capybara::Element) find_by_id(id, options = {})
- (Capybara::Element) find_field(locator, options = {}) (also: #field_labeled)
- (Capybara::Element) find_link(locator, options = {})

* The `find_*` methods are all simple wrappers for `find`
* `find` based methods will wait for `default_wait_time` seconds if the driver supports JS
* `first` is a wrapper for `all`

Question: his blog post indicates that #first and #all do not wait

## 13 RSpec Matchers

* These all express expectations about the state of the `page` object.
* They are smart enough to wait up to `default_wait_time` if the don't succeed the first time they try.
* The are all convenience constructors for `HaveSelector.new(...stuff...)`
* They are handy in cases where a particular element is on the page but its contents are loaded aysnchronously

- (Object) become_closed(options = {})
- (Object) have_button(locator, options = {})
- (Object) have_checked_field(locator, options = {})
- (Object) have_css(css, options = {})
- (Object) have_field(locator, options = {})
- (Object) have_link(locator, options = {})
- (Object) have_select(locator, options = {})
- (Object) have_selector(*args)
- (Object) have_table(locator, options = {})
- (Object) have_text(*args) (also: #have_content)
- (Object) have_title(title, options = {})
- (Object) have_unchecked_field(locator, options = {})
- (Object) have_xpath(xpath, options = {})


## 22 Node methods

* These are stubbed out in capybara but must be implemented by whatever driver you use
* It seems like drivers subclass Driver::Node
* => they have a different implementation in different drivers

- (Object) ==(other)
- (Object) [](name)
- (Object) all_text
- (Boolean) checked?
- (Object) click
- (Boolean) disabled?
- (Object) double_click
- (Object) drag_to(element)
- (Object) hover
- (Node) initialize(driver, native) constructor
- (Object) inspect
- (Object) path
- (Object) right_click
- (Object) select_option
- (Boolean) selected?
- (Object) set(value, options = {})
- (Object) tag_name
- (Object) trigger(event)
- (Object) unselect_option
- (Object) value
- (Boolean) visible?
- (Object) visible_text
