# Privacy

There are 3 categories of privacy we will consider:

1. intance variable privacy
2. method privacy
3. property privacy (properties are shorthand for defining methods so this is
   basically the same as _method privacy_.


## Instance variable priacy

* ObjC objects have instance variables (`ivar` for short)
* By default they are _protected_ but you can change this

```
// Define *the* interface for the class
@interface

    NSString *iAmProtected; // default

    @public
    NSString *iAmPublic;

    @private
    NSString *iAmPrivate;

    @packaged
    NSString *iAmPublicOniOSAndFrameworkPrivateOnMac;

    @protected
    NSString *iAmAlsoProtected; // this class and its children only

@end
```

## Method and property privacy

* Properties are just a convenient way to declare methods so what applies to
  method privacy applies to them too.

```objc
// This syntax actually creates a "class extension"
@interface MyClass ()
    // stuff that is put in this "class extension" is private to just the class
@end
```
We can have a read-only property in our class's interface that we redeclare as
read-write in the class extension.

## Undeclared methods

There is no such thing as public _methods_ or private _methods_ in ObjC
(public/private/protected instance variables is definintely a thing). If you
declare a method in your public interface you are telling the compiler (and
other devs) that it is OK to call this from other classes.

You can use undeclared methods in your class implementation but it is not a good idea because:

    ??? not sure if these are correct
    The compiler can't check the selector name for you so you can easily type it
    If you make an error calling an undeclared method then it will fail at runtime not compile time


## Difference between instance variables and properties

* An instance variable is literally just a storage slot for data
* a property is a more abstract concept - it is a "possibly computed value"
  because all access is through the igetter/setter.

Consider

    @property (nonatomic,strong) NSSTring *foo;

This indicates that there is a thing in this object that you can treat as if it
were an NSSTring instance even though it may be calculated. you can access it
through

    [self foo]
    [self setFoo:@"hi"]

or using the dot syntax you can do

    self.foo
    self.foo = @"hi"

which makes it totally look like a vanilla NSString

## Examples of accessing instance variables

```objc
@interface Foo
{
    @public
    NSString *bar;
}
@property (strong, nonatomic) NSString *someProp;
@end

@implementation Foo
    - (void)someMethod
    {
        bar             // direct access the ivar
        self->bar       // direct access the ivar
        _someProp       // direct access the ivar made by the property
        [self someProp] // access via getter
        self.someProp   // access via getter
    }

@end

// Create an instance of Foo
[[Foo alloc] init];

// Access foo's ivar directly
foo->bar;
//foo.bar; // ERROR: `.` sends a message so only works for properties!

// Aside this is consistent with C struct access
foo->bar; // get the "bar" member of the struct pointed to by foo

// properties make methods so can be accessed via message sends
foo.someProp    // access via getter
[foo someProp]  // access via getter

```

## Properties vs ivars

accessing instance variables via properties is almost always better than direct
ivar access.

* pro: properties can be overridden to be computed in future
* equal: performance of properties is very slightly lower but [almost never
  noticible](http://www.bignerdranch.com/blog/should-i-use-a-property-or-an-instance-variable/)
* pro: properties provide a single place to set a breakpoint for access/change when debugging

