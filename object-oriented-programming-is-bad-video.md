## OO is bad video

https://www.youtube.com/watch?v=QM1iUe6IofM

His thesis: OO programming produces abstrastions taht aren't good, there is no particular problem with abstraction

procedural = no explicit association between data types and functions
imperitive = we mutate state whenever we feel like it (no special handling of shared state)
functional = minimize the mutation of shared state
object oriented

He outlines the style choices as follows

    procedural vs OO
    imperitive vs functional


He says that programming started out procedural + imperitive but that the shared state became a problem as program size grew
Two solutions were presented

1. procedural + functional = minimize shared state (get rid of it as much as possible)
2. OO + imperitive = segregate shared state - don't make it global, break it out into objects 

We can both minimize shared state (funcitonal style) and then segregate what state is left 
    * he thinks segregating state is fine strategy up to a certain level of detail

styles of coding

* procedural and imperitive
* procedural and functional (minimize state by having most functions be pure)
* Object oreitnted and imperitive (segregate state, divide and conquer the state problem)
* object oriented and functional (do both)

in the past as programs grew there was a need to model something larger than an individual function or datatype
object came to fill this need but they are really just a combination of the two

* he belives that _sometimes_ you want to have a really clear association between some functions and a data type but that doing this by default is very bad

he states

* abstraction is a worthy goal
* good abstractions are very hard to create
* OO tends to produce abstractions very prolifically
* if most abstractions are not good and we have a lot of abstractions then he contends that OO leads us down a bad path

He describes OO programming as "abstraction heavy" programming and states that while it's stated goals are

* structure
* maintainability
* flexibiliyt
* readibility
* elegance
* simplicity

it does not achieve them.

Aside: It is true that most of the programming stuff I read cites these as the goals but the **proof** that OO delivers them is not cut and dried.

He says that OO delivers the illusion of the above things not the reality
## Managing state

BW thinks we should 

1. minimize state
2. only then segregate what state we have left
3. not break stte up into bits that are too small

and contends that OO does not emphasise minimizing state enough


# Polymorphism

polymorphism is a thing which all styles can have - you can have procedural polymorphism (how???)
you can have polymorphic code in procedural code

# Encapsulation

a message send is NOT a method call

* message send
    * sends a copy of all state - they do NOT references
    * sends and returns information about state, not state itself
    * => messages cannot pass around object references
    * Aside: using structs in swift gets us closer to "message sends" because they are passed by value

> it is better to start with an absence of structure rather than impose one that will not fit our problem

There are a no. of graphs in OO

1. inheritance heirarchy
2. data flow graph
3. method call graph
4. composition graph

In procedural code there is only the call graph

* strictly speaking message sends should send _copies of all state_ - they should NOT send references
* he contends that encapsulation does not work _at a fine grained level_
* for an object to send messages to another object it has to have a reference
  to it so OO programming can’t really work by sending copies of state
* if A has a privately held reference to B then that B is part of the state of
  A if we are to follow the principle of encapsulation properly
* messages may indirectly read and modify state
* => if two objects have a reference to a third object then they have shared state!
* as soon as you objects being shared encapsulation is gone

* pure functions are the only truly self contained unit of code

* he recommends encapsulation at the level of packages/namespaces/modules etc.

# Modularization

* in attempting to modularlise and label every fiddly bit that our program
  does we are actually just making it hard to understand
    * he has a point here: naming is hard and bad names are probably worse than
      no names in some cases
    * Where is the sweet spot of naming? 
        * pulling out methods can help with understanding
        * but it does fragment the flow of control
* in splitting up our code into many small methods and classes are we
  decreasing the complexity of our code or just diplacing the complexity and
  spreading it around?









