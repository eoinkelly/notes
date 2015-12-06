# Ruby Tips

```ruby
def foo(*)
  # ignore all args
  # Why does this work?
  # I guess *foo is a splat arg so * is a splat arg you can't get hold of
end
```
