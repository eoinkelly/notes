### Rails Controllers Part I video

my review:

* a decent continuation of the previous video

Questions for rose

* Show me the updates to your git repo after watching this video
* What does REST stand for?
* What is REST?
* What does CRUD stand for?
* What is a RESTful route?
* Is rails RESTful?
* He recommends not moving outside the 7 normal actions - when would you not want to do this?
* Following conventions whenever possible is important in rails. What code (that we have seen already) _depends_ on the  rails convention of path helper names (answer: form_for in the view). Bonus q: how do you override #form_for to use a differently named path?
* Why is it better to use path helpers rather than raw paths in #redirect_to calls in the controller?
* Have a go at converting the ERB to Haml.
    * Can you have ERB and Haml in the same project at the same time? If so, how does rails tell them apart?
* where would you find a reference for the data types you can use in the rails model generator?
* Find the rails guide which shows you exactly what methods get added to your class when you add a `belongs_to` association.
* Rails seems to be good at converting symbols from singular to plural and vice versa. You can do this in your own code too - what methods in rails help you do this?
* When he creates the nested routes (cards nested inside decks) why does the output of `rake routes` list all the cards routes first?
* Why is working with a model through an association a good idea? (A: it filters when searching, automatically sets up associations when creating)
* Show me the cards controller you made in your git repo

(A: so that the more general deck/:id routes don't match first - the routeing file is checked top to bottom and rails stops when it finds something that matches)


