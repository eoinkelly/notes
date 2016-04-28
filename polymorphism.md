# Polymorphism

Aside: To "parameterize" is to favour passing data into a function via function parameters vs some other means e.g. global variables

Polymorphism is the provision of a single interface to entities of different types.

A polymorphic type is one whose operations can also be applied to values of some other type, or types.

Three types

1. Ad hoc polymorphism:
    * when a function denotes different and potentially heterogeneous
      implementations depending on a limited range of individually specified
      types and combinations. 
    * Ad hoc polymorphism is supported in many languages using function overloading.
1. Parametric polymorphism:
    * when code is written without mention of any specific type and thus can be used transparently with any number of new types. 
    * The concept of parametric polymorphism applies to both data types and functions.
    * In the object-oriented programming community, this is often known as generics or generic programming. 
    * In the functional programming community, this is often shortened to polymorphism.
1. Subtyping (also called subtype polymorphism or inclusion polymorphism): 
    * when a name denotes instances of many different classes related by some common superclass.
    * In the object-oriented programming community, this is often simply referred to as polymorphism.
    * Some languages employ the idea of subtyping (also called subtype
      polymorphism or inclusion polymorphism) to restrict the range of types
      that can be used in a particular case of polymorphism

Implicit type conversion has also been defined as a form of polymorphism, referred to as "coercion polymorphism"


## Static or dynamic

* Polymorphism can be distinguished by when the implementation is selected:
  statically (at compile time) or dynamically (at run time, typically via a
  virtual function). 
* This is known respectively as static dispatch and dynamic dispatch, and the
  corresponding forms of polymorphism are accordingly called static
  polymorphism and dynamic polymorphism.
