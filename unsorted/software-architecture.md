# Inbox

* http://blog.8thlight.com/uncle-bob/2012/08/13/the-clean-architecture.html (has links to other architectures too)

# SOLID Principles

* SRP   Single Responsibility
	* There should never be more than one reason for a class to change
* OCP   Open/closed Principle
	* A module should be open for extension but closed for modification
* LSP   Liskov Substitution Principle
	* Subclasses should be subsitubale for their base classes
* ISP   Interface Segregation Principle
	* Many client specific interfaces are better than one general purpose interface
	* Meitz thinks you only need this in a staticly typed language. She thinks its not a problem in ruby
* DIP   Dependency Inversion Principle
	* Depend oupon abstractions. Do not depend on concretions.

* Introduced by Micheal Feathers
* These principels tell you a state that you are in when everything is right.

# sandy video

rigid apps everything is connected to everythign else. a single change causes a cascade of related changes.
fragile apps are rigid but you can't tell by looking
immobile app = code is hopelessly intagled, you can't reuse anything, you can only re-use by duplication (copy & paste)
viscous app = behaving badly is the most attractive alternative (cupboard full of badly stacked tupperware)
these come from the robert marting Design Principles and Design Patterns paper

The SOLID princples

Design is ALL about dependencies
* If you refer to something, you depend on it.
* When the things you depnd on change, you have to change


# Dependency Inversion Principle

* traditionally lower-level components are designed to be consumed by high-level compontents. This means that the high-level components depend on the low-level ones
* This makes it difficult to re-use the high-level components.
* What if the high level component defined (and owned) an "interface" that defined what services it needed from the lower-level compontent
* The high-level component just calls the interface (which it owns) so no dependency there
* The low-level component implements the interface so it depends on the high-level component because it has to know things about the interface like funciton names, param lists etc.
* So the low-level component depends on part of the high-level component (inverted dependency)
* We also need some way at runtime of tying them together. Patterns such as:
    * Plugin
    * Service Locator
    * Dependency Injection
  are used to wire the high-level component and chosen low-level component together at run-time
* Now the high-level component can use any low-level component that implements the interface.
* For this to work it's important that the interface be "owned" by the high-level component
* The DIP can also be seen as applying the "Adaptor" Pattern
