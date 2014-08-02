### Rails Controllers Part II video

#### Questions for student

* Are there any security concerns with the way you have written your controllers?
* Consider your controller code through the lens of security.
    * Are there any potential problems?
    * What potentially risky jobs do controllers (in general, not just yours) have?
    * What can we do to mitigate these problems?
* He abstracts out the `find_deck` method in the `CardsController`.
    * What are the benefits of doing this?
    * What are the disadvantages of doing this?
    * Given the benefits and disadavantages you have come up with, would you abstract it?
    * Tutor note: what i'm trying to get at here is the trade off between creating an abstraction and removing duplication
* You will notice that we are assigning a few `@instance_variables in our controller now (for magic copying to the view) - if assiging 2 is ok, what do you think of a controller that assings 100? Where would you draw the line?
    * Bonus: what strategies could we use to avoid having so many instance variables?
* in `Cards#new` he creates a new card object via `@deck.cards.new`. Is this the exact same object we get when the user submits the form to the `#create` method?
    * Hint: Remember `#object_id` on an object to check its unique ID.
* view partials
  * partials are good when you have a chunk of template that you want to use in multiple places (removes duplication)
      * when would you _not_ use a partial if you see some view template that is the same?
  * Are partials good at making a complex template simpler (even if there is no duplication)?
      * Tutor: trying to show the diff between creating abstrction and just moving code around
  * Notice how he has explicitly passed in variables into the partial - this is more typing for you but what is the advantage of this?
      * Do you have to explicitly pass in variables to a partial (hint: try it out)?
      * What are the advantages of each approach?
* What is the difference between `resources` and `resource` in the routes file.
  What is the heuristic for knowing which one to use?
  ANSWER: In routes file, resources is for things that have an id - if it
          doesn't have an id you should use resource
* He returns current_user from `signed_in?` method - I don't think this is
  correct - I think you should explicitly return a boolean.
#### Notes

He uses a pattern of passing the form builder into the partial, not having the
whole thing be in a partial - this is quite nice.

* if you pass #render an object, it will look for a partial with the same name e.g.
    render wombat # loads "_wombat.html.haml"
    * not sure how useful this is?

* he is very against adding extra actions to controllers (outside of the 7) e.g.
  rather than adding #sign_in to UsersController he suggests
  SessionsController#create i guess this is more restful

* form_for can take a symbol which does ???

```ruby
form_for(:session) do |form|
  ...
```

he recommends putting the helper_method call right after the method defn.
because you only have one place to look - i think this is quite nice.

* research & doc what `helper_method` does in controllers
    * it makes the method you give it available to the views as a helper
    * ??? if you don't put it in the ApplicationController, do you get per view
      helpers from it?

* is he correct in saying that cookie.sign will also encrypt the cookie ???

In routes file, resources is for things that have an id - if it doesn't have an
id you should use resource




