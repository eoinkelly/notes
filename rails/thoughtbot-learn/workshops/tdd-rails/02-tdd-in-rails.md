# Writing integration tests

## Questions for Rose

- What suffix do all testing files have?
    - Why do we do this?
- What does the `randomised seed` stuff mean in the output of Rspec?
- what is the difference between `feature "stuff" do ... end` and
  `describe "stuff" do ... end`
- he prefers HTML5 attributes over CSS for finding stuff on your page - why?
- He uses Rspec `expect` syntax. Rspec also has `should` syntax but stylish
  testers don't use it anymore - why?
- What is your overall impression of the workflow with tests? Notice that is
  very similar to the earlier stuff they did working form the outside in the
  app.

- Have they created a complete set of test for this app? If not, is this OK?
  What would you do?
- Refactoring seems to be a lot about inserting more levels of indirection.
  Won't this make the code harder to understand?
- He uses a normal method to make a helper method.
    - What does `let` and `let!` do in Rspec?
    - Is it better to use `let`/`let!` or make methods?
- What is a "mystery guest" (hint: search the thoughtbot blog)
- He says in one place that you shouldn't manipulate the DB directly from specs
  but then he does it anyway. What do you think of his choices here?

# Notes

I feel I would miss opportunities for controllers to get models through a
relationship - this gets me automatic scopeing e.g. how he got the todo's via
current_user. I think I would have probably written a query off Todo.where...

Note: he commits after green, then refactors

TODO for me: dig deeper into capybara docs, understand the types of things it
deals in e.g. nodes, locators, finders etc.

```
# an idiom for checking that an element is on the page and has expected content
expect(page).to have_css ".foo", text: "some content"
expect(page).to have_css ".foo", text: "some content", visible: true, count: 1

page.has_css?([:kind], locator, options)
    :kind = :css|:xpath
    locator = a css/xpath string that will find stuff on the page
    options = any of
        count: 4
        maximum:
        minimum:
        exact:
        between:
        text: "some content"
        visible: true|false
```
