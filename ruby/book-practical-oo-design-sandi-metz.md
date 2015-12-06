# Chapter 1 - Object Oriented Design

* OO design solves problems; suffering from those problems is very nearly a prerequisite for comprehending these solutions.

* In almost all cases, maintainability over the life of the code is more important than optimizing its present state.

* Object-oriented design (OOD) requires that you shift from thinking of the world as a collection of predefined procedures to modeling the world as a series of messages that pass between objects.







* Each OO application gradually becomes a unique programming language that is specifically tailored to your domain.

* Do these refactorings even when you do not know the ultimate design. They are needed, not because the design is clear, but because it isn’t. You do not have to know where you’re going to use good design practices to get there. Good practices reveal design.


* Changing requirements are the programming equivalent of friction and gravity. They introduce forces that apply sudden and unexpected pressures

* Designs that anticipate specific future requirements almost always end badly. Practical design does not anticipate what will happen to your application, it merely accepts that something will and that, in the present, you cannot know what. It doesn’t guess the future; it preserves your options for accommodating the future. It doesn’t choose; it leaves you room to move.

* The purpose of design is to allow you to do design later and its primary goal is to reduce the cost of change.

* This book is not about patterns; however, it will prepare you to understand them and give you the knowledge to choose and use them appropriately.

* object-oriented software fails when the act of design is separated from the act of programming.
When design is dictated from afar none of the necessary adjustments can occur and early failures of understanding get cemented into the code.

* design takes time and therefore costs money. Because your goal is to write software with the lowest cost per feature, your decision about how much design to do depends on two things: your skills and your timeframe. If design takes half your time this month and does not start returning dividends for a year, it may not be worth it. When the act of design prevents software from being delivered on time, you have lost. Delivering half of a well-designed application might be the same as delivering no application at all. However, if design takes half of your time this morning, pays that time back this afternoon, and then continues to provide benefits for the lifetime of the application, you get a kind of daily compounding interest on your time; this design effort pays off forever.

* In Ruby an object may have many types, one of which will always come from its class. Knowledge of an object’s type(s) therefore lets you have expectations about the messages to which it responds.

* The String class manufactures new strings, the Class class manufactures new classes.

### Questions to evaluate an object Foo

public interface
* how many messages in it's public interface
* how many of it's private messages/data does it expose?

dependencies
* How many other objects in the system does Foo's code reference the names of? (remember that classes are objects too) - each one is a dependency.
* How many messages does it know about for each object (how *much* does Foo know about the other object)
* Does it know anything about how other objects relate to each other e.g. Blah.foo.goo.boo?

dependents
* can't see that from looking at the code for Foo
* could find it by grepping the code-base




# 7 OOD Principles

Single
O
L
I
D
LoD
DRY


SRP
===

Why multiple responsibilities is bad
* We accept that we don't know what future changes will be required
* So we _WILL_ (no doubt about this if you accept the point above) need to remix our code in unexpected (by defn.) ways
* So our code needs to be made of blocks that can be remixed and rewired.
* It should be possible to wire up these blocks today into the app we need today but the blocks should be flexible enough to be remixed.
* This means our classes have to be easy for our brains to understand - it should be easy to look at it adn say "This does X, and I need X elsewhere so I can safely use it to do X"

techniques to figure out if a class or method has a single responsibility:

1. pretend the class or method is sentient & phrase each one of it's methods as a question.
	does the question make sense for that class/method?
	is there more than one question?
2. try to describe their responsibilities in a single sentence.
	if the simplest sentence you can come up with has "and" the class probably does more than one thing
	if the sentence has "or", the class probably has more than one responsibility and they aren't even related!

How can you determine if the Gear class contains behavior that belongs somewhere else? One way is to pretend that it’s sentient and to interrogate it. If you rephrase every one of its methods as a question, asking the question ought to make sense. For example, “Please Mr. Gear, what is your ratio?” seems perfectly reasonable, while “Please Mr. Gear, what are your gear_inches?” is on shaky ground, and “Please Mr. Gear, what is your tire (size)?” is just downright ridiculous.

Don’t resist the idea that “what is your tire?” is a question that can legitimately be asked. From inside the Gear class, tire may feel like a different kind of thing than ratio or gear_inches, but that means nothing. From the point of view of every other object, anything that Gear can respond to is just another message. If Gear responds to it, someone will send it, and that sender may be in for a rude surprise when Gear changes.

Another way to hone in on what a class is actually doing is to attempt to describe it in one sentence. Remember that a class should do the smallest possible useful thing. That thing ought to be simple to describe. If the simplest description you can devise uses the word “and,” the class likely has more than one responsibility. If it uses the word “or,” then the class has more than one responsibility and they aren’t even very related.

* Separating iteration from the action that’s being performed on each element is a common case of multiple responsibility that is easy to recognize.


# Chapter 4 Creating Flexible Interfaces

interface can mean 2 things
1. the set of methods within a single class that are designed to be used by others
2. spans across classes and is independent of any single class. It is just a set of messages

A public interface is made of methods that
* Reveal the primary responsibility of the class
	* If the class has a single purpose (as it should) then these methods should read like a description of responsibilites that fulfill that purpose
	* The public interface is a *contract* that articulates the responsibilities of the class
* Are expected to be invoked by others
* Will not change on a whilm
* Are safe for others to depend on
* Are thoroughly documented in the tests

All other methods in a class are part of its private interface. They
* Handle implementation details
* Are not expected to be sent by other object
* Can change for any reason whatsoever
* Are unsafe for others to depend on
* May not even be referenced by the tests

* Object oriented systems are defined by the messages that pass, not the classes
* An OO system is made up of classes but defined by messages.
* Classes are what's in the source code repo, messages reflect the living, breathing app.

# Starting a design

* When you have some use-cases you will start by having some "domain object" in mind - these are the big, visible, real-world things that your app will model. Domain objects are easy to find but they are NOT AT THE CENTRE OF YOUR APP!
* Design experts notice domain objects without fixating on them. They focus not on these objects but on the messages that pass between them. By focusing on the messages you reveal the other objects which your app needs, other objects which are just as necessary but far less obvious.
* So domain objects give you a starting point for figuring out the *messages* that your app should pass, then we design around the messages
* Use sequence diagrams to experiement with what objects should be in the system
* The pattern seems to be that your domain objects lead you to identify the messages you need to pass. Once you know the message you can create the appropriate receiver for it - we have objects becuase we have messages!

## Sequence Diagrams

* Sequence diagrams are artifacts that you throw away once they have clarified your ideas
* They hep you figure out "Should this receiver be responsible for responding to this message?"
* Sequence diagrams let you **discover** the public interfaces of your classes.
* They help you decide on what messages to send and then figure out who should receive them

## Single Responisbility, Simple Context

Objects should have a single responisbility and a simple context
context = the stuff that has to already be established in the system before you can use this object
* I guess testing is a good way to measure the context required for a class - if you need to do a heap of setup for tests then you probably have a big context
* Objects with simple context are easy to re-use and easy to test.
* The best possible situation is for an object to be completely independent of it's context
* Dependency injection is a good way to reduce the context - it means that the object doesn't have to know what other object will receive the message it wants to send
* A method can send self as an argument to a message which kind of says "Hey I want you to do X to the object I pass"
Communicating the stablity "likelyhood of change" of my methods to outsiders is important. You can do that with protected, private, public keywords but ruby lets  you override these anyway so you can also do it with a naming convention e.g. _some_method() for private methods (Rails does this)

SM believes that communicating this stability is the reason for the private, protected, public keywords

* Assume that other devs have created their class interfaces intentionally and learn to read what they are telling me with their use of the visibility keywords. Don't use a "private"/unstable method if I can avoid it
* When you depend on an unstable interface you increase the risk of being forced to change. Avoid them (especially on external frameworks)
* If I can't avoid depending on a private interface, I should minimise the potential damage by isolating the dependency into a class or method within my own code.

## Dealing with a bad public interface on a class

If I am faced with a public interface that forces me to know a lot about "how" it does things (rather than me just telling it what to do) I can still create a better public interface for it by
	* create a wrapper class with a good public interface - this way the rest of my code is insulated from it
	* Add new methods to the offending class that give it a better interface
	* Add a wrapping method to all my classes that use it
Which of the above we choose depends on how often I will use the class in my code.

# Law of Demeter

* A set of coding rules that result in loosely coupled objects
* "only talk to your immediate neighbours" or "use only one dot" (it is more subtle than this tho)

For example

class Trip
	def depart
		customer.bicycle.wheel.tire # retrieves a distant attribute
		customer.bicycle.wheel.rotate # invokes distant behaviour
		hash.keys.sort.join

	end
end

* If wheel changes it's tire or rotate methods then depart() might have to change. Changes in wheel might force changes in Trip. For example, in 6 months you might change how rotate() works - would you expect that to break depart()?
* Trip now expects to know about a customer object which has a bicycle which has a wheel which knows how to rotate. This is a lot of "context" for Trip. This means you can't reuse Trip in any system that doesn't have these objects (with that exact relationship). In effect I have coupled the depart method to my apps object structure i.e. depart knows that customers have bicycles which have wheels which have tires etc.
* There is an argument in favor of getting distant attributes as the cost of removing the violation might be higher.
* No such argument exists if you plan to change the value of that attribute - in that case you are implementing behaviour that should be in wheel
* train wrecks of demeter violations are clues that there are objects whose public interface is lacking

hash.keys returns an Enumerable
hash.keys.sort also returns an Enumerable
hash.keys.sort.join returns a String (or an Enumerable of Strings)

All of the intermediate objects have the same type. There is no Demeter violation.

* You can use delegation to hide demeter violations but using delegation to hide tight coupling isn't the same as decoupling the code!
* Demeter violations occur when you are unduly influenced by objects you already know - you know lots about the public interfaces of existing objects so it's tempting to just reach on in there and grab what you need. You want code that just knows what it wants but you also end up with it knowing *how* to navigate through a bunch of intermediated objects to get it

# Chapter 9 - Designing Cost Effective Tests

Writing changeable code is an art which depends on 3 skills
1. Understand OO design
	* From a practical point of view, changeability is the only metric that matters
	* Code that is easy to change *is* well designed.
2. Be skilled at refactoring code
	* Refactoring: Improving the design of existing code
	* Refactoring is the process of changing a software system in such a way that it does not alter the external behavior of the code yet improves the internal structure
	* A precise process that alters code via crab-like steps and incrementally transforms one design into another.
		? is there a "half-way done" point in refactoring where things are worse than before you started?
		? or is the point to make the steps small and discrete?
3. Write high-value tests
	* Tests give you the confidence to refactor constantly
	* Efficient tests prove that altered code continues to behave correctly without raising overall costs
	* Refactorings should not force rewrites of the tests

	Well designed code is easy to change
	refactoring is how you change from one design to the next
	tests free you to refactor with impunity
* Notice that the above implies that my code will go through many "correct" designs as requiremetns change