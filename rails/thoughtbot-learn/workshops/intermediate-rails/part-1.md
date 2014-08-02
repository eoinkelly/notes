# Intermediate rails part 1

# Questions for rose

* he begins by deciding some data models here wants
* what are the common group names in the Gemfile?
* what is the advantage of making groups in your Gemfile?
    ANSWER: you can manage groups separately, can exclude certain groups, rails
    does this too!
* Rails scaffolds are great. Any reason you should not use them?
    A: don't leave empty/placeholder files in your repo
        understand everything the generator makes! Magic is only good if you
        know what is going on! :-)
* What are the pros/cons of specifying versions in your Gemfile?

* Discuss: Notice how he builds  out the app, starting at the route -> controller ->
  model, getting errors all along the way and letting the errors guide him.
  Compare this with starting from the models and working your way out. What are
  the trade-offs? (rembember: trade off implies that there are both pros and
  cons)
    * notice that he does this a few times - he starts with a plain english
      description of what the users should be able to do and works his way
      backwards.
    * what popular/recently controversial coding practice does this resemble?
* Todo: make a new migration manually (without using a generator) that does
  exactly the same as the rails generator. Do the same for the model generator
* When you change initializers you have to restart your server because
  initializers are only checked at server startup.
    * How does rails know when you other files have changed?
    * Isn't this a terrible idea for an app in production? How does rails solve
      this?
* Why would you want to keep the number of public methods in your class low?
* What is the difference between `resource` and `resources` in the routes file?
* Why does he recommend only adding the routes you are currently using (using
  except ...) - wouldn't it be easier to just create all the routes especially
  since it is easier for you/less typing?

* He uses the `kill` command to stop the rails server - what does the `-INT`
  parameter to kill do?
* Do you think the rails `div_for` is a good idea? What are the pros/cons?
* What is polymorphism?

## Notes

investigate the monban gem - compare to devise

```ruby
root to: 'home#show', via: :get # a nice little limitation to GET
```

read up on `rexource` vs `resources`

"if the object you are talking about doesn't have an id then it is a singular
resource so you should use `resource` not `resources` in the routes

resource creates 6 routes not 7 - it does not make #index
Idea: a script that will figure out which routes are defined in routes.rb that
don't exist as controller actions

default_scope in rails 4 will require a block
research what default_scope does



