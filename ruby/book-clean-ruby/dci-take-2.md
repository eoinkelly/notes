Times
* design & first write time
    * we want to be able to reason bout the program here
* program runtime
* maintain time 
    * we want to be able to reason bout the program here


Things old OO is good at

* capturing state
    * at write-time you create attributes in your class
    * at maintain-time you only have one place to look to find out about
      everything that class does (hopefully!). this makes it easy to reason
      about
    * easy questions " what state is associated with objects of this type?"

* expression operations associated with that state
    * these operations don't involve collaborators
    * easy questions "what operations can be performed on the state?" easy
      because all the operations are in one place (the class). even if other
      parts of the system invoke the operation at runtime you can
    * hard questions "who invokes these operations while the system is running?"
            * hard because you potentially have to look at every other file in
              the system

  Things OO is bad at
  * capturing interactions between objects
  * hard questions "who the fuck calls my methods at runtime?"


After a while our models become bags of functionality that is vaguely related to
the state in the model There isn't any one place in the system to look at the
algorightm as a whole e.g. user registration we are forced to go sperlunking in
code, going from method to method trying to infer what was intended based on
method & variable names this is very hard esp if methods have a high complexity
(large no. of possible paths through them)


Downsides of DCI
* we modify the class heirarchy at run time which means all the stuff ruby had
  cached gets blown away def obj.method; end class << self; end extend
  define_method

  arguments against it http://tonyarcieri.com/dci-in-ruby-is-completely-broken
  (it kills method cache)

There are a few issues here

* Is DCI a good idea? Probably
* Is DCI just the Delegator pattern?
* Can we do it in Ruby? Yes
* Is it worth the downsides?


DCI is about much more than just extending objects at runtime. It's about
capturing the end user's mental model and reconstructing that into maintainable
code. It's an outside → in approach, similar to BDD, where we regard the user
interaction first and the data model second. 

DCI is about more than code - it is a design process


The flow of DCI (with movie methaphor stuff)

* start with your user story (the tagline)
* flesh it out into a use case (the script)
    * the script has actors, scenarios start and end points, roles (actors are
      taught to play these)

DCI Paper 
=========

* http://www.artima.com/articles/dci_visionP.html

OO was supposed to be about capturing mental models

There is a problem with the user not being able to discern what mental model ot
use for the operation e.g. insert image into paragraph of text in Word but I'm
not sure how much that can be solved by DCI

In standard OO the algorightms (methods) are put iwht the objects they show the
most affinity for.  This works fine for a single object but is a bit shit if the
algorithm involves many objects becase we have to split the algorightm across
many objects

Consider an Account class which has two abilities
1. Decrease it's balance
2. Widthdraw money

Decreasing a balance is 
    * a "characteristic of the data"
    * "what it is"
    * a stable operation D

Performing a withdrawal is 
    * "the purpose of the data"
    * "what it does"
    * a relatively unstable operation
    * a behaviour of the **system** - it far outstrips any data model

The two abilities are very different when viewed from any of these prespectives
* System architechture
    * ???
* Software engineering
    * ???
* Maintenance rate of change

Traditional OO lumps both abilities into the same bucket


My objects should depend on objects that are more stable than them.  If all my
objects are supposed to be stable where do I put the parts of the system that
change?

I know that there are parts of my system taht are stable (change infrequently)
and parts that unstable.  A key decision I have to make when designing is where
to put the stable bits and where to put the unstable bits This implies that I
should rate functionality based on it's stability when I am designing ???
Traditional OO kind of forces you to put the stable and unstable bits together

One approach to this problem in traditional OO is to use inheritance:
    * Put the stable stuff in the parent
    * Put the unstable stuff in the children This approach is the "open-closed
      principle"

This paper distinguishes between a class and an object "the class became the
implementation tool for the analysis concept called an object" I think this
means that we became too focused on static stuff (what we could see in our
editor) and ignored dynamic (what actually happens when the program runs)
behaviour

"The discord between the algorithm structure and the domain structure would be
the ultimate undoing of classes as units of growth" interesting ideas: domain
structure algorithm structure

Consider the example of a SavingsAccount class it can certainly
increase/decrease and report it's balance what happens when I want to add
"withdrawals" to the system?  where do I put the logic?  I make some new objects
to handle it and add some extra skills to SavingsAccount I would make a
            * controller /withdrawal. It might coordinate the whole thing


The paper makes the point that roles are how humans think about systems. I have
a general model of what a funds transfer means in my head independant of what
kind of accounts are involved. DCI attempts to model **that**

Objects capture what objects are roles capbure collections of behaviours about
what objects do The authors believe that roles are unfamiliar to us because our
existing languages are miserable at expressing them


We still do procedural decomposition when we are breaking down an algorightm!
* In traditional OO we create boundaries to the decomposition along object lines
    * Object boundaries also are used as boundaries for the data - this dual use
      is confusing because we are forcing ourselves to break down our
      algorightms along the same boundaries we break down the data!!!
    * In DCI we create boundaries along role lines

Video 
======


DCI shines when there is a sequence of operations

Use cases
* they must have a business goal at the end

They distinguish roles and classes almost as if roles existed in the sytem as a
thing like classes ``` Role SourceAccount ...  end ```

having a business goal this the defining characteristic of a use-case!  A
usecase = a collection of scenarios between objects that achieve a business goal
in context Their design process generates "classes" and "roles"
* event based operations = atomic operations without a goal
* classes = good at modeling the domain objects and simple "event-based"
  programming
* roles = a collection of related responsibilities, good a modeling the use
  cases
* algorightms = steps that represent sequencing steps imposed by the
  implementation
    * the usecase can be a bit more vague that the final code in some areas e.g.
      which account initiates a transfer of funds
* habits = common bits of action sequences that don't have a business goal, can
  be re-used in many use-cases. a use-case can contain habits

Software only has value in its running, not when it is just sitting on the
machine e.g. softwre is a service

Book: domain driven design - might be helpful for understading how to model the
domain objects (not necessairly the algorithms)

They believe that smalltalk & C++ are both force us into class oriented
programming

Class composition --------------

* The techniques we use in DCI are "class composition" - this is different to
  the "object composition that we are used to

object composition = an object can hold references to other objects (to whom it
can make calls on) class composition = the stuff in an object is composed of
bits from multiple classes and modules

The class level relationship and the object-level relationship **are different**

A person object "has a" heart object but Person does not "have a" Heart class

When you create instance vars in a context object they represent "roles"

Ruby's #extend doesn't really do class composition (it does it on an instance by
instance basis) but in other langs such as Scala it is achieved by real class
composition - these "mixed in" modules are called traits in some languages

``` Class MoneyTransfer def initialize(src, dst, amt) # these instance vars are
**roles** @source_acc = src @destination_acc = dst @amount = amt end end ```

"contexts stack"

"A full use case presented in one file would be too hard to understand" ???
really

We have 2 cognitive techniques to manage complexity
1. divide and conquer
    * split the big use case up into smaller use cases that we can understand
2. compression
    * using sub routines in a program is compression, not abstraction
    * an abstraction implies that we are removing stuff too

"smart domain objects don't evolve well"

A context can behave like a domain object - it is just another kind of object a
context can treat other contexts as objects contexts are just stateless objects
- DCI tries to formalise this stuff a bit

"DCI is a combination of lean and agile"

The domain models should only contain stable things - the more dynamic things
should be in the roles domain model = "what the system is" (tends to be stable)
roles = "what the system does" (tends to be unstable)

most roles in DCI are stateless - the state is remembered by the domain objects

"A use case is 90% exceptional cases - it contains all the extenstions and
variations"

Books - Peter Coad: he perhaps has some interesting ideas on this

Trygve talk 
===========

"Java is not object orientated, neither is Simula" WHen reading a program we
like to have exection time go down the page

Dijkstra didn't believe in testing - he didn't believe you could test quality
into code, it had to be part of the code

Testing can be used to show the presence of bugs but never to show their absence

"The use of subroutines is not abstraction it is compression. Abstractions
involves removing stuff to simplify - we don't want to do this at coding."


He talks about an example of building ship software back in the 60's.

* Trygve loves code review for improving the quality of code
* Trygve say
    * get code right first time
    * test to confirm no bugs (not to inject quality), must execute all
      statements
* if you didn't think of it when you were writing the code, you won't think to
  check for it in your tests
* Their culture was that the person who was responsible for the correctness of
  the code is the reviewer
* They spent a lot of time talking to the customers in the shipyard (so they got
  a good understanding of the context the software would be used in)

His overview of code from the 60's:

1. Data-centric architecture for separation of state and behaviour
2. Application design with functional composition
3. Textual flow represents process flow
    * ideally we want the flow of our representation of the code to match what
      happens at runtime
4. Peer review of code chunks to get it right first time
5. Testing as a no-blunder confirmation

His overview of code from 71-95
1. Class centric architecture with encapsulated state and behaviour
2. System design with functional fragmentation
    * contrast this with "functional decomposition"
3. Code fragmented, does not show program flow
4. No chunks, no peer review
5. Code correctness can only be explored by tests

Coplien feels like agile people have lost some important disiplines along the
way

"procedure orientated" vs "object orientated"
-----------

* procedure orientated code
    * the client code has to know the type of an object it is working on so that
      it can call the right method
      ```
      deleteCircle()
      deleteGroup()
      deleteRectangle()
      ```
* object orientated code
    * the client code cannot know the type of the object it is working on."This
      is really an abstraction becasue we are throwing away info" (???)
      ```
      object.delete()
      ```
classes tell us lots about what an object does when it gets a message but
nothing about where the message came from or why it arrived!

TR says you can't send a class out for code review because each class has parts
that come from many different use-cases

The user has a mental model of use cases and **run time** behaviour of the
system - the don't think in terms of "what classess make up this system"
The programmer has a mental model of classes, attributes, methods
There is a **big** gap between these models!

The most important thing in an OO system is the spaces between objects

A context can be considered as a package or roles

Contexts prevent polymorhphism when programming in the large - we can still do
it in the small

The difference between procedural and OO tinking
    * "this happens and then that happens and then the other happens"

OO thinking: 
    * thinking about *who* does things "A does this and then B does that and
      then C does the other"

The separation of system state (in the data classes) and system behaviour (in
the context classes)
TR believes that DCI is very similar to "responsibility driven design"
It also seems to be similar (but not the same as) aspect orientated programming

The key to DCI is the injection of parts of the algorightm into objects so that
you don't have to add heaps of stuff to the object's interface


Do podcast
======

JG makes the point that roles that have the same name in multiple contexts are
not necessairly the same e.g. "admin" in Context A might not have any methods in
common with "admin" in Context B (even if admin is a trained-up from the same
User class)

They are useing "role player" not "actor" as terminology so it doesn't confuse
the actor pattern

take a play script, and take all the parts for romeo and put them into one file
and give them to an actor - it's a bit shit because he doesn't have any context

rails concerns are just copy-paste refactoring

JG doesn't think the performance thing is enough to not use DCI - he says he has
used #extend in production without problem

It's probably better to wrap **How** you do the role assignment so that you can
change you mind later


Service objects are when you extract a controller code into a helper module.
