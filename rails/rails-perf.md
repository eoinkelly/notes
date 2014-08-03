# Rails Performance

##  http://confreaks.com/videos/4107-rdrc2014-speed-up-rails-speed-up-your-code

* Typically we trade off time and memory space when improving performance
    * Implications:
        * To improve speed, we use more RAM
        * To improve RAM usage, we go slower
* There is also the possibility of finding a better algorightm but this is less
  common.

### Performance measuring tools

* What do we measure?
    * We measure the performance of individual methods.

* ? Does it make sense to measure performance of a class?

#### `benchmark/ips`

* iterations per second
* provides standar deviations
* you give it a block to run as many times as it can in X seconds
* ruby has `benchmark` gem built-in.
* Things to watch out for
* Noise - how much other work is your machine doing at the same time? How much
  does this change between benchmarks
    * It is handy to have a standard deviation here so you can see how noisy
      your result is.
    "say the slowest was X and the fastest Y and it was mostly in the middle at Z"
    TODO refresh std dev
    * `benchmark/ips` can show you std dev


### `stackprof`

???

### `GC.stat()`

```ruby
GC.stat(:total_allocated_objects) # num of objects allocated in the system since start
```

???

### `allocation_tracer` gem

???

### Tracepoint (built in to ruby)

???

### Rails 4.2 (AdequateRecord) ActiveRecord::Relation cache

???


### How ActiveRecord works

```ruby
_img/Screenshot 2014-08-02 22.36.13.png

# When you do ...
Post.find(33)
# ... it becomes instance of ...
ActiveRecord::Relation
# ... becomes ...
Arel::SQL::Node
# ... becomes a SQL query string "SELECT * FROM xyz etc." ...
# ... which gets sent to the DB ...
# ... the results of which come back and become instances of ...
ActiveRecord::Base
```

### Rails 4.2 caching

* Rails 4.2 caches
    * the ActiveRecord::Relation - it can use the same relation with different values.
    * the compiled SQL
* Rails does type casting of the things you pass to `#find` e.g if you search
on the ID column, AR will notice that it is an Integer in the DB so will cast
that value to an Int.
* Anything that uses relations will use this cache e.g.

```ruby
Post.find()
Post.find_by_*
Post.find_by({name: value, ...})
has_many
has_many through:
has_and_belongs_to_many
belongs_to

# NBThere are a bunch of ways to call Post.find() that cannot be cached!
# The only one that can be cached is to pass an integer!
# You can do
Post.find([1])
Post.find([[1]])
Post.find(post)
Post.find({id: 1})
etc.
```

* Note that `#where` is not on the list above!
* I should use find or find_by* whenever possible over #where because it is
  faster
* Rails 4.2 has removed HABTM and replaced it with a HasMany through
  because HABTM is a subset
* Rails 4.2 trades memory for speed - the cache's take up extra memory


### HTML Sanitization in Rails

* ActiveSupport::SafeBuffer instances are considered safe
* Raw strings are not considered safe
* `html_safe` just tags the string - it does mean the string *is* safe!


### Freeze strings

In Ruby 2.1+ every time the compiler sees a string literal with `#freeze` called
on it, it will return the exact same object (with same object_id)


### Conclusion: To speed up rails app

* Cache invariants. Anytime you can find a calculation you are doing
  repeatedly and always returns same results for same inputs you should cache it
* Eliminate objects allocations
* Limit types - means less run-time checking and defensive programming
