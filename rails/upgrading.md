
# Updating rails

http://railsapps.github.io/updating-rails.html

```sh
cd path/to/project

# get the most recent versions of the tools you'll use
gem update --system
gem update bundler

# create new branch to work on
git checkout -b upgrade-rails

# get outdated gems that are actually in your Gemfile (not just deps of
# something in there)
bundle outdated | grep group
# example output:

# simplecov (newest 0.10.0, installed 0.9.1) in groups "development, test"
# show outdated production gems only
bundle outdated | grep default

# update rails configuration and binstubs
rake rails:update
```

