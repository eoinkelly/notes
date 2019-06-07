# How does Rails render templates?

Sources

* https://youtu.be/8Dld5kFWGCc (@tenderlove 2019 Railsconf keynote)

ActionView has 3 jobs

1. Find templates
1. Compile templates
1. Executing views


Templates are implemented in ERB

ERB
* developerd by @m_seki
* built into ruby stdlib
* there are multiple implementations of ERB
    1. Original ERB
        * Still what you get in the Ruby stdlib
    1. ERubis (faster than ERB)
        * Rails used to use this
        * hasn't been updated since 2011
    1. ERubi (faster still, used by default in Rails 5.1+)
        * https://github.com/jeremyevans/erubi

ERB works as follows:

Templates are

1. Read off disk
2. Compiled into a String of ruby code (available as #src on a template)

The string of ruby code is passed to `eval` with a binding in which it can lookup methods and ivars and the output of that `eval()` is the rendered template

Implications

* ERB doesn't cache anything for you. If it works for your use case, you can cache

1. The raw template string read off disk
2. The compiled template string (of ruby code)
3. The final rendered output


Rails "caches" the compiled template by dynamically defining a uniquiely named "render" method whose body is the ruby code created by compiling the template.

This turns looking up the cache into a method call which is pretty fast.

It seems like Rails puts multiple of these dynamically created "render" methods on the same class - this means that any ivars defined in the templates are shared between all the render methods

Implications

* Rails has to dynamically create a "render" method name
    * it builds it based on
        1. start with an `_`
        1. the path to the template on disk (with filesystem separators replaced by `_`
        1. add some numbers to the end of the name
            * not sure what these are - maybe file mtimes? or some other cache busting
    * you see these method names if you put a pry in a view or a view raises an exception
* Templates are translated into methods on a particular class


Passing local variables to partials

Rails creates a "preamble" section in the generated method which sets tehe local variables based on the hash you passed to the partial


```ruby
def render_long_name_here(locals)
  name = locals[:name]
  bar = locals[:bar]

  _erbout = +''
  _erbout.<< "...."
  # ...
  _erbout
end

```

a challenge with this is that rails can't know whether the `<%= name %>` in your template refers to a local or a method when evaluated so it's impossible to precompile templates once and re-use them - the context might be different at each eval so we have to eval every time - we can cache the output of that eval'ing but we can't reuse the cached output.

Also this means that if you have a partial that takes a variable number of locals then youmight get multiple copies of the compiled method.

Tip: Try to always pass the same locals to each template each time - it will help ensure that the template won't be compiled multiple times.

