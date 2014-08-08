# Callbacks

TL;DR Only use AR callbacks if you can't avoid it.

* Thoughtbot consider them a code smell

* pros
  * easy to use
* cons


* sanitizing user input
    * is probably better in a form object or controller
    * the model's job is just to validate that it is getting good stuff -
    * the sanitizing should be done somewhere else!

Things that you can put in callbacks

Counter cache
    * whenever the things you are countin is created, you update the cache (in
      after_save)

Why are callbacks dangerous

You are tying the action you are doing in the callback to the lifecycle of the
object so it is no longer possible to perform that action at any other time
it traps the logic inside that class
you can't use, reuse, test, play in console without also _saving the record_
it makes that logic hard to test
It makes it scary to manually do things in the console to the objects because
you are afraid of triggering the callbacks
All the callback logic is hidden behind what you think is just adjusting an
attribute on a model. Seems terrible.

persistence should be treated as its own thing

just because things usually happen at the same time does not mean they are the
same action.
just because you usually send an email when the user's details are updated does
not mean that creating and sending that email is the same thing as saving new
details.

Callbacks and transactions are not good friends - your callback code will run
even if the transaction gets rolled back for whatever reason

TODO: investigate AR #transaction

How not to use them

1. You can use a decorator that wraps your AR object. Call #save on the decorator
  which does stuff including calling save in the AR object.
    * This layers the extra functionality on to the model without having the
    * model have knowledge of it.
2. If you need to save a bunch of things consier creating a new object to manage
it e.g. a form object for data coming from a form or some sort of "saver" object
for other stuff
3. An event aggregator or pub/sub pattern might be useful
    ways that events are different to AR callbacks:
      * events are best used for side effects - they will not be able to modify
      * the return value of a save operation (unlike a callback)

AR Observers have been removed from rails 4 - TODO find out more
```ruby
```
