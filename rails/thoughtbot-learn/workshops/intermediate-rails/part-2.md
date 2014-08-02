# Intermediate Rails Pt 2.

## Questions for rose

* When should you use single table inheritance - what is his sentence for
  remembering when to do it?
* What is polymorphism?
* Notice again how he goes about making the app - he works from the routes
  inwards, letting the error message be his guide.
* when he restarts his rails server he passes the `-d` parameter. What does this
  do?
* "write what you want and then figure out how to make it work" - work your way
  in from the outside. There are two classes in ruby that are really handy for
  quickly making new objects - they are Struct and `OpenStruct` - what is the
  difference between these? What are their strengths and weaknesses?
* Show me some example code you have written that demonstrates the difference
  between Struct and OpenStruct
## Notes

He really wants you to stick to the 7 resourceful actions in controllers
Think of what you want to happen in terms of
I am getting|putting|deleting|creating  NOUNNAME
and choose NOUNNAME appropriately

```
# will render _foo.html.haml once for each element of @foos
render @foos
```


```
# bad example of using STI
class Shout < ActiveRecord::Base
    # shout data
    # shout behaviour
end

class TextShout < Shout
    # TextShout data (different to Shout data)
end

class ImageShout < Shout
    # ImageShout data (different to Shout data)
end
```

* He believes STI works best with "models that will have the exact same data but
  different behaviour"
    * if the data is different we don't want to use STI
* If your models have different data then there will be a lot of NULL columns in
  the table
    * why is this bad?

```
class Owner
end

class Pet < ActiveRecord::Base
end

class Fish < Pet
    # unique fishy behaviour
end
class Cat < Pet
    # unique catty behaviour
end

# table would look like
# id name owner_id created_at
```

he thinks about his data models as db columns first

```
# if shout.content is a polymorphic relationship, render will render the partial
# name based on the type - this is a pretty neat pattern!
render shout.content
```

Your helper modules are mixed into the view class.


If I am the teacher, the the _reflexive association_ is that you are the
student.


TODO: play with STI in a dummy rails app
