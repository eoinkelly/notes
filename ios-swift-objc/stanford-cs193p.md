# Lecture 1

* 10 week course
* seems to be approx 18 lectures

## 4 layers in iOS architecture

1. Core OS (most of this layer is written in C)
    * OS X kernel
    * Power management
    * Keychain access
    * Mach2 (kernel)
    * BSD
    * Certificates
    * Sockets
    * Filesystem
    * Security
    * Bonjour
2. Core services (OO i.e. Obj-C from here up)
    * collections
    * Address book
        * odd that this is here, not in media layer?
    * Networking
    * File access
    * SQLite
    * Core location
    * Core data (Object orientated database)
    * Net services
    * Threading
    * Preferences
    * URL Utilities ???
3. Media (isn't covered much in this course)
    * Core audio
    * OpenAL
    * Audio Mixing
    * Audio Recording
    * Video playback
    * JPEG, PNG, TIFF
    * PDF
    * Quartz 2D
    * Core animation
    * OpenGL ES
4. Cocoa touch (most of course in here)
    * multi touch
    * core motion
    * view heirarchy
    * localisation
    * controls
    * alerts
    * web view
    * map kit
    * image picker
    * camera


A _property_ of a controller that is a reference to a view is called an _outlet_

In iOS MVC, models and views should *never* know about each other

Views can only communicate with controller in a "blind" and "structured" way

* "blind" because it can only send messages to whatever controller it is given
  a reference to. A view cannot send messages to arbitrary controllers
* "structured" because all views send messages to all controllers the same way

# 2 Ways that controllers and views communicate

### 1. Target/action

Controller marks (a method??) as a "target" and then it gives the view an "action" (?some sort of reference to that method??)

### 2. Delegate protocol

The view instance has a reference to a delegate object that will have
implemented a (protocol) bunch of methods it can call to send messages to e.g.

* Hey the user _will_ start scrolling soon!
* _Should_ I allow the user to scroll?
* The user just _did_ a scroll

That delegate object is the controller. The pattern in the messages is

* did ...
* should ...
* will ...

A protocol is just a "blind way to communicate with another object"

Views can have references to another kind of delgate called a "data source".
This delegate implements a protocol that answers questions about the data e.g.

* Give me the data at ...
* How many elements in ...

The controller behaves as this delegate for the view too

### Views do not own data they display

Views do not own the data they display - they are not the source of truth for
that property (same idea as not keeping truth in the DOM)


## How the model communicates with controller

The controller has a reference to the model but the model does not have a
reference to the controller.

Sometimes the model will need to tell the view things wihtout the view first
asking a question. To allow this we use a sort of "radio station" model.

Model broadcasts info to anyone who is interested - this is _Notification_ and
_Key value observing_


# Combining MVCs

* He refers to "an MVC". This is 1 model instance, 1 controller instance, and 1
  view instance all wired up as above.
* A large app is made up of multiple "MVCs" working together.
* An MVC can use another MVC as its view.
    * This is the main way a large iOS app is structured.
    * => controllers have delgate relationships ???

This is quite different to the Rails flavour of MVC.

## Objective C

* A strict _superset_ of C

### ObjC Properties

* similar to attr_accessor in ruby
* we rarely access instance variables directly in ObjC
* a property is a getter + setter pair that accesses a value
* naming convention:
    * getter: just the name of the value e.g. `myValue`
    * setter: `setMyValue`

.h file is the _public API_ of the class
.m is the implementation of the class (of both public and private methods)


```objc
// *********************************************
// card.h
// *********************************************

// Import all the public interfaces of all classes in Foundation framework
@import "Foundation" // magical syntax for Apple frameworks
#import <Foundation/Foundation.h> // old school way

// begin the public interface of Card which inherits from NSObject ...
// couls also say NSObject is the _superclass_ of Card
@interface Card : NSObject

// properties can be either strong|weak
// strong => keep this in the heap as long as at least one strong pointer left
// weak   => keep this in memory as long as somebody else has a storn pointer to
//           it. If it runs out of strong pointers set this pointer to nil.
// nonatomic => this property is not thread safe
// If we don't specify 'nonatomic' the generated getter/setter for the property
// will contain a bunch of locking code to make it work across threads
@property (strong, nonatomic) NSString *contents;

// Notice we can override the getter name to make it read nicer
// * We don't need strong|weak here because this BOOL is a primitive value and
//   is statically allocated (not on the heap)
@property (nonatomic, getter=isMatched) BOOL matched;
@property (nonatomic, getter=isChosen) BOOL chosen;

// Public methods of Card
- (int)match:(NSArray *)cards;


@end // end of public interface
```



```
// *********************************************
// card.m
// *********************************************

// Import the public interface of our class
#import "Card.h"

// Begin the private interface of our class
@interface Card()
@end

// begin the implementation of all interfaces of our class
@implementation Card

// *********************************************
// Automatically created by ObjC via @property
// *********************************************

// The following line says
//   "_contents is the name of the instance variable where the property 'contents' is going to be stored".
// or
//   "synthesize property contents to use _contents as storage"
// It allocates space for the contents in the object.
// "synth-e-size"
@synthesize contents = _contents;

// getter
- (NSString *) contents
{
    return _contents;
}

// setter
- (void) setContents:(NSString *)newContents
{
    _contents = newContents;
}
// *********************************************

- (int) match:(NSArray *)cards
{
    // score is primitive local so is stack allocated
    int score = 0;

    // if ([[card contents] isEqualToString:[self contents]]) {

    // We only use "dot notation" for calling the getter/setter of properties!
    // Although you can use it to send a message with 0 args to any object it is
    // bad form to do so.
    for (Card *card in cards) {
        if ([card.contents isEqualToString:self.contents]) {
            score = 1;
        }
    }

    return score;
}

@end // end of implementation block
```

Objective C

* does _not_ require you to declare methods before you use them in a file.
* _all_ objects live on the heap! Consequence:
    * any variable you have to an object is _always_ a pointer.
* We can (and do) send messages to `nil` pointers in ObjC
    * It does not result in a nul pointer exception as it would in other langs
* All properties start off as `nil` when you declare them
    * => you don't have to worry about them pointing at arbitrary memory

NSArray
    * is hetrogenous - it can contain any object
    * is immutable (use NSMutableArray as mutable version)

NSUInteger
    * will be different on different platforms e.g. on iPhone 5+ it will be 64bit
    * is an alias for `unsigned long *` (a pointer to a long on the heap)


 QUESTION: what shoudl you do if you don't have a higher res version of an image? add the low res in the slot or jsut leave slot empty?


# Lecture 3

## Semi-private properties

A property can be readonly in the public API but read-write in the private API.
In ruby this would be:

```ruby
class Foo
  attr_reader :my_prop

  private
  attr_writer :my_prop
end
```

## Default memory config for properties

The defaults for properties is

strong
    * only applies if object is heap allocated
readwrite
    * properties make both a getter and a setter by default


There is no "protected" in ObjC - only "public" and "private"


arr[4]
is sugar for
[arr insertObjectAtSubstringIndex:4]

`NSArray` and `NSMutableArray` cannot be sparse but you can insert `[NSNull null]` objects to give the appearance of a sparse array.



## Designated initializers

* If you subclass a class you have to call its designated initializer e.g. `[super initWithBlah...]`

Outlet collections have no ordering
    * it does not matter what orcder you drag UI elements onto the property


# Lecture 4

Ways we create objects on the heap

1. alloc and init
    NSMutableArray *cards = [[NSMutableArray alloc] init];
2. class methods
        + (id) stringWithFormat:(NSString *)format ...
3. Ask objects to create new objects for you
        - (NSString *)stringByAppendingString:(NSString *)otherString;
        - (id)mutableCopy;

Sometimes both a _class creator_ method and some init methods exist

    [NSString stringWithFormat:@"foo"]
    // is exactly same as
    [[NSString alloc] initWithFormat:@"foo"]

Both are there for historical reasons. Before ARC the class methods were handy
because they had good semantics for how memory was allocated.

Tutor favours alloc-init rather than class methods if possible

Not all objects given out by other objects are newly created

Unless the method has the word "copy" in it then you get a pointer to an existing object if one exists. If they object does not exist you are creating it e.g.
    * NSArray has `- (id)lastObject;`

## nil

* we can send messages to nil
    * if the method returns a value, it will return 0

```
int i = [obj methodWhichReturnsInt]; // i will be 0 if obj is nil
```

Be careful if the method returns a C struct because the value is undefined
You do not get back a struct with all its members set to 0 - you might get stack garbage

```
CGPoint p = [obj getLocation]; // p is undefined if obj is nil
```

Dynamic binding

