# Notes

- again he works from the outside in, letting the errors be his guide

"a controller should know _what_ to do but not _how_ to do it"

being able to work with ruby's enumerable is pretty vital for being able to work
iwth active record

his refactoring show how handy ruby's blurring of local variable and method
names

He uses `@_foo` as a naming converntion to indicate _private_ instance variables
in the controller. is there a better way of achieving that? He memoizes `@_user`
but the downside of rails' magic is that this will get passed to the view.

- His `@dashboard` is kind of a presenter but is also a domain object
- It seems like a nice pattern to come up with a _thing which is responsible for
  the presentation of info_ that doesn't end in `Presenter`
- He calls his dashboard a service object, I would have used presenter
- When he is making a decorator but again it is also a domain object - he is not
  calling it `FooDecorator`

- Any ActiveModel compliant object can make a partial path for itself - he uses
  this to good effect when splitting up the views.

There is a 1:1 connection between the top-level methods on his presenter and
view partials

=> A presenter can/should expose activemodel compliant things in its public
methods

What you really want to avoid is violating LoD in the view:
`@presenter.thing.other_thing`

### ActiveModel concerns

- It seems like concerns just add sugar for the pattern that AR modules already
  use.
- I strongly disagree with his use of concerns here - he has just moved the code
  around - there is no new abstraction. He also currently has no re-use so it is
  premature to pull this out.
    - I guess there is an argument that you have created a "named place to put
      stuff". This only works if the next dev will not have to read through all
      the concerns to understand whatever they need to change (i.e. we have
      created an abstraction).
        - Is this likely within a model?
        - Do concerns encourage you to make your models bigger when you should
          really be pulling stuff out into other objects?
            - you can't really pull out associations etc. so maybe concerns do
              have a place

# Questions for Rose

- List the different kinds of associations rails can make for you.
    - Explain the relationships that each one can be used to model (hint: some
      associations are used in more than one kind of relationship)
- What is memoizing? Type me a method that memoizes the return value of an
  existing method.
- What does `||=` do in ruby? What is the long-hand version of it?
- What is the rails _flash_?
- He passes a `notice` to the `redirect` method in the controller. What is going
  on here? Explain to me in detail 1. What `notice` is refering to 2. how could
  I set it in a controller action that does not use redirect 3. There are a few
  other "level" of notice I can send the user, what are they? 4. How exactly
  would I make use of notice in the view templates?
- What are Sandi Metz's rules? List them
    - Discuss the pros/cons of each of the rules.
- Teach me about the S.O.L.I.D. principles?
    - Discuss each of the 5 ideas encapsulated by SOLID
- He mentions design patters a few times? What is the canonical book on design
  patterns? Where can you get your hands on it?
- Is it ok to have models in `app/models` that do not descend from
  `ActiveRecord`?
- He refers to refactorings in this video. What is the canonical book on
  refactoring code? Find it on the rabid bookshelp
- Explain the law of demeter to me.
- Show me your code of your cleaned up `TextShout` and `PhotoShout` controllers
- At the end he pulls stuff out of the `User` model into a concern
    - Ruby already has `module` - how are modules different from concerns?
    - What are the pros/cons of rails model concerns?
