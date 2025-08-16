# How to read Rails 6 allocation counts

- The number of allocations is reported for each template partial
- The allocations include the allocations of all children of that partial

Allocations is calculated running:

```ruby
GC.stat :total_allocated_objects
```

at start and end of event and then subtracts them.

Details:
https://github.com/rails/rails/blob/master/activesupport/lib/active_support/notifications/instrumenter.rb#L159
