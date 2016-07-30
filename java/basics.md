# Java language

General

* types have uppercase names

## static

* makes the variable/function shared between **all** instances of the class
* makes the member belong to the type, not the instances of the objects.

## final

* A final variable can only be initialized once
* A final method cannot be overridden or hidden by subclasses
    * prevents subclasses from doing anything dodgy with the method
    * the difference between `final` and a constant is
        * a constant's value is known at compile time
        * a final variable value may not be known until runtime
* if the member points at an object then the contents of the object can be
  mutated but the reference cannot be changed.

## package

* a named group of related classes
* default class visibililty is to be "package private" i.e. visible only to
  other classes in the same package
* IDEs want to organise classes by package (creating a new subdir for each `.`)
    * TODO: why?
* If your src file does not declare a package it is considered to be in the
  "default package"
    * TODO find out more

## access control

* https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html

* classes contain members (a term which includes both methods and data)
*

* there are two levels of access control
1. top level
    * applied to classes themselves
    * there are two options for access control here
        1. package private (default, no modifier keyword)
        2. public (selected by prefixing class name with `public` keyword)
            * makes the class visible to **all** other classes in the app
2. member level
    * there are ? options for access control here
        1. package private (default, no modifier keyword)
            * this member can be accessed from any class in its package
            * this member is **not** available to subclasses of this class unless they are in the same package
                * QUESTION: what is the use case for subclasses in a different package?
            * NOTE: there is no keyword for "package private" - it is indicated only by the lack of an access keyword
        2. public (selected by prefixing class name with `public` keyword)
            * this member can be accessed by any class in any package in the system
        3. private
            * this member can only be accessed from within its own class
        4. protected
            * this member can only be accessed by
                1. any class in its package
                2. any subclass of its class in another package
            * => protected is "package private + subclasses of this class in any package"


## nested classes

* https://docs.oracle.com/javase/tutorial/java/javaOO/nested.html
* nested class is a member of its enclosing class
    * as such the normal access control keywords apply to it
* two types of nested class
    1. non-static nested class aka "inner class"
        * has access to **all** members of the enclosing class even if they are declared `private`!
        * is associated with an instance of the outer class
            * => it cannot declare any static variables itself
        * variables with the same name in the inner class as the outer class will shadow the variable in the outer name.
            * you can access the outer class variable if you need to via `OuterThing.this.varName`
        * Syntax for instantiating these is ugly: `OuterClass.InnerClass innerObject = outerObject.new InnerClass()`
            * note that we call `new` on an instance of `OuterClass` not `OuterClass`
    2. static nested classes
        * they don't have any special access to the other members of the enclosing class
        * is behaviouraly exactly the same as any other top level class - it has just been nested to imply some link to the outer class
        * Syntax for instantiating these is straightforward: `OuterClass.InnerClass x = new OuterClass.InnerClass()`

## interfaces

* signify a "role" that an object can play
* a class can implement multiple interfaces i.e. play multiple roles
* can only contain
    1. constants
    2. method signatures aka "abstract methods"
    3. default methods
        * can have method body
        * have the `default` modifier as prefix
        * when an interface extends an interface that has a default method you can
            * redeclare it (makes the original default method abstract)
            * redefine it (completely overrides the default method)
        * can be redeclared (overridden) by classes that implement the interface
        * can be redefined by
    4. static methods
        * can have method body
    5. nested types
* interfaces cannot have fields (data members)!
* interfaces cannot be instantiated
* they can be implemented by classes
* they can be extended by other interfaces
* an interface can be used as a data type (anywhere you could use a class)
* you can make changes to an interface over time by
    1. using interface extnsion to make a new or specialised version of the interface while not breaking existing client code
    1. use default methods to add new methods to an interface and not break existing code

## super

* super is a keyword
* it lets you access methods from a superclass
* `super.foo()` will search through the inheritance hierarchy (starting with the superclass of the current class) and try to find the method.
* In constructors `super(<args>)` will call the appropraite constructor from the superclass


## Object

* Similar to Ruby, `Object` is the parent of the java class heirarchy


## null

* can be returned from a method no matter waht the return type is declared as


## finalizers

* TL;DR - almost never use them.
* there may be a long delay before they are actually called by the VM

