### Rails Models video

my review:
    * decent basic intro to rails
    * Interesting: he starts with a migration for his model before he makes any model

Questions for rose

* Show me your repo of code you wrote for this video
* Tell me about the MVC pattern in your own words. What are the jobs of each part?
* Does all business logic have to go in the models? Where else might it go?
* Tell me about helpers - what are they?
    * Why do rails generators create a new file there for each controller?
* What is the "one sentence" overview of how bundler works?
* Migrations:
    * can you think of any limitations of migrations? Are there cases you should not use them?
    * Are migrations good for changing the structure of database
    * Are they good for adding content to the database?
    * Where is the resource that shows you exactly what column types rails will create
* why would we want a primary key on a database table?

* What are the differences between running IRB in your app directory and rails console?
* What is mass assignment protection?
    * ME: how is this different in Rails4 with strong params?
* what does `protect_from_forgery` do?
* what is the difference between methods and actions in a controller?
* How many routes did we end up creating in these videos? (Hint: they are very common)
* What is the difference between a `Hash` object and `HashWithIndifferentAccess`?
* It's not mentioned in the video but what does the SOLID acronym stand for in Object Oriented design?

NEW TO ME:
* I didn't know that #view_assigns was the thing that took instance variables in the controller and passed them to the view so it could create instance variables from them.
* you can test routes in console using `app.route_helper_name(any params)`
* Rails4 uses `patch` for updates not `put`
* What is diff between `update_attributes` and `update` ???
* rails maps HTTP verb DELETE to #destroy for historical reasons - we need to stick with it
* TODO: become "performance fit" with strong parameters.


