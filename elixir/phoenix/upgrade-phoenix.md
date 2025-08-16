To upgrade a phoenix project:

```
mix hex.outdated # see what needs updating
# edit mix.exs to reflect the constraints you want
mix deps.unlock --all
mix deps.update
mix deps.compile
```
