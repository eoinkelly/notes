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
3. Ask objects to create new objects for you with their instance methods e.g.
        - (NSString *)stringByAppendingString:(NSString *)otherString;
        - (id)mutableCopy;

Sometimes both a _class creator_ method and some init methods exist

    [NSString stringWithFormat:@"foo"]
    // is exactly same as
    [[NSString alloc] initWithFormat:@"foo"]

* Both are there for historical reasons. Before ARC the class methods were handy
because they had good semantics for how memory was allocated.
* The tutor here favours alloc-init rather than class methods if possible.

ObjC has a naming convention for guessing the memory handlying semantics of a method correctly:

Not all objects given out by other objects are **newly** created!

* Method name does not have _copy_
    * you get a pointer to an existing object if one exists. A new object is
      created if one does not exist e.g. NSArray has `- (id)lastObject;`
* Method name does have _copy_ in name
    * always give you a new object!

## nil

* we can send messages to nil
* Sending messages to nil is (mostly) ok - no code is executed

What does a method called on nil return?

* if the method normally returns a value, it will return 0 - it is ok to rely on this
* CAREFUL: if the method returns a C struct because the value is **undefined**!
    * You do **not** get back a struct with all its members set to 0 - you might get stack garbage

```
// obj is nil
int i = [obj methodWhichReturnsInt]; // i will be 0 if obj is nil
CGPoint p = [obj getLocation]; // p is undefined if obj is nil
```

## Dynamic binding

### id type

* ObjC `id` is a "pointer to an object of unknown/unspecified type"
* pronounced "eye-dee"
* consequences
    * it is already a pointer  - `id *` makes no sense in ObjC because we do not do pointers to pointers.
All pointers are treated like `id` at runtime because ObjC has dynamic binding i.e. it does not decide what code to execute in response to a message until right before it sends the message at runtime e.g. `NSString *` is same as `id` at runtime

At compile time things like `NSString *` are good because the compiler can help you find bugs
Consequences:
    * if you explicitly use `id` type in your code then the compiler cannot help you!
    * if you were to use `id` everywhere in your code then it would be more like the situation in ruby! ;-)

```
NSString *s = @"foo" // legal and can get type help from compiler
id obj = s; // legal, no warning from compiler
NSArray *a = obj // legal but a terrible idea, no warning from compiler
```
ObjC uses _static_ typing and _dynamic_ binding.

```
id myObject;
```

The compiler will warn you if the method you are sending is not defined in your program but if it is defined anywhere then the compiler has to assume that what you are sending it to _might_ be a thing that responds to it.

Typecasting does not execute any code - it just tricks the compiler

Use cases for `id`

1. An hetrogenous NSArray
2. The "blind, structured" communication between controllers and views. The view is sending messages to the delegate it got but it does not know the type of that delgate!

The ways we can protect ourselves are

1. Introspection
2. Protocols

We do use explicit type casting (which also leaves the compiler blind) and use id sometimes but we do so when we have protection.

### Introspection

We ask the object that we have the pointer for what methods it will respond to.

`NSObject` has a number of introspection methods:

```
isKindOfClass       Will search up the inheritance heirarchy
isMemberOfClass     Will not search the inheritance heirarchy
respondsToSelector
```

* The answers to these questions are calculated at _runtime_!
* All introspection methods take a `Class` object as parameter. As in Ruby, get the class of an object by sending the `class` message e.g. `[foo class]`

```objc
if ([obj isKindOfClass:[NSString class]]) {
    // notice we explicitly cast obj to what we know it to be now
    // QUESTION: the cast seems a little redundant here?
    NSString *s = [(NSString *)obj stringByAppendingString:@"xxx"];
}
```

respondsToSelector takes a selector (`SEL`) type. The special `@selector()` _directive_ turns the name of a method into a selector

```objc
if ([obj respondsToSelector:@selector(shoot)] {
    [obj shoot];
} else if ([obj respondsToSelector:@selector(shootAt:)]) // note the
    [obj shootAt:target];
}

```

`SEL` is a typedef like `BOOL`. You can delare instances of `SEL`

```objc
SEL shootSelector = @selector(shoot);
SEL shootAtSelector = @selector(shootAt:); // <-- includes the :
```

`NSObject` has some methods that explicitly take selectors as arguments:

```objc
[obj performSelector:shootSelector];
[obj performSelector:shootAtSelector withObject:coordinate];
// Limitation: you can only send messages with 0 or 1 args this way
```

NSArray has `makeObjectsPerformSelector` which will make each object in the
array perform the given selector. This is a nice (but not as nice as a `map`) way of running things on all elements of an array.

```objc
NSArray *ary = ...
[ary makeObjectsPerformSelector:shootSelector];
```

Another use case for both `id` and `SEL` is when you are programmatically
adding a target to a view. The view does not know the type of the delgate but
we do need to tell it what message to send to the delgate.

```objc
UIButton *button;
- (void) button addTarget:(id)target action:@selector(isPressed:) ...
```

### Protocols

* are inbetween id (most unsafe) and static typing (most safe)
* We don't specify the class of an object but do specify what methods it should implement
* example:
```
id <UIScrollViewDelegate> scrollViewDelegate;
```

_will be covered more later_

## Foundation framework


NSObjec timplements `description` message

We use it in

1. NSLog the @% returns it
2. We implement it in our own classes

NSObject also stubs out

```objc
- (id) copy         // give me an immutable copy of this object
- (id) mutableCopy  // give me a mutable copy of this object
```

which are implemented differently by different classes (and will raise an exception if they don't exist).

Note that `copy` will give you an immutable version of a mutable thing if the recierver you send it to is mutable i.e. it is a bit like `freeze` in ruby.

Making copies of collection classes (`NSArray`, `NSDictionary`) is very efficient so don't sweat doing so.

* NSArray
    * all objects held `strong`ly
    * is immutable - only objects that are put in there when created will be
      there.
    * Has shorthand syntax `@[]`
* NSMutableArray
    * inherits from NSArray
    * usually created with alloc+init
    ```
    [[NSMutableArray alloc] init];
    [NSMutableArray array]; // shorthand as above

    // Class methods
    - (id)arrayWithCapacity:(NSUInteger *)numItems; // capacity is just a perf hint to the runtime


    // Instance methods
    -(void)insertObject(id)object atIndex:(NSUInteger *)index
    -(void)removeObjectatIndex:(NSUInteger *)index

    NSMutableArray *a = [NSMutableArray array];
    [a addObject:@"a"];
    [a addObject:@"b"];
    NSLog(@"Array is: %@", a);

    [a setObject:@"foo" atIndexedSubscript:0];
    a[1] = @"bar"; // same as line above

    NSLog(@"first: %@", [a objectAtIndexedSubscript:0]);
    NSLog(@"first: %@", a[0]); // same as line above

    NSLog(@"Array is: %@", a);
    ```

You iterate through an array using `for-in`. Because arrays are hetrogenous you are in essence casting each value as you pull it out

```objc
NSArray *hopefullyStrings = ...

for (NSString *s in hopefullyStrings) {
    // do stuff, possibly crash if you get something expected
}

// For arrays that contain mixed things you can use `id` type and use
// introspection to figure out exactly what it is.
for (id thing in hopefullyStrings) {
    if ([thing isKindOf:[SomeClass class]) {
        // do thing
    }
}
```

### NSNumber

`NSNumber` is an object wrapper around C scalar numeric primitive types:

* a signed or unsigned char
* double
* enum
* float
* int
* long int
* long long int
* short int

```objc
NSNumber *n = [NSNumber numberWithInt:36];
float f = [n floatValue]; // 36.0 Notice that NSNumber can convert between types
```
We usually want to wrap them so we can put them in an array or dictionary. The short-hand syntax for making NSNumber is `@()`

```
NSNumber *foo = @3 // can omit () for simple numbers
NSNumber underline = @(NSUnderlineStyleSingle); // enum to NSNumber
NSNumber val = @([card match:@[otherCard]]); // convert primitive return value to NSNumber
```

NSNumber also provides a `compare` method for comparing these primitive types


### NSValue

`NSValue` is a wrapper for non primitive, non object types e.g. C structs

* NSNumber inherits from it
* Not used much in this course

A good strategy for working with C structs in ObjC is to turn it to/from a string when you need to put it in arrays etc.

```
+ strings are easy to work with e.g. put in array/dictionary
- slightly less performant than NSValue
+ ObjC has good function for converting between C structs and strings
```

### NSData

* just a bag of bits
### NSDate

* use to find current date or store past and future dates
* see also NSCalendar, NSDateFormatter, NSDateComponents
* If you are going to put date UI you have to be aware of localization issues

### NSSet

* Also NSMutableSet
* like an array but
    1. no ordering
    2. all members are unique
* can union and intersect with other sets

### NSOrderedSet

* Cross between array and set
* Objects have an order but are also distinct i.e. you can't put the same object in multiple times
* probably faster for finding things than array


## NSDictionary

* An immutable collection of key value pairs
* keys and values are both objects

```objc
// @{ key: value, key2: value2, key3: value3 }

NSDictionary *colors = { @"green": [UIColor greenColor],
                         @"red": [UIColor redColor],
                         @"blue": [UIColor blueColor] }

UIColor *col = colors[@"green"]; // lookup object with []
```

* All keys and values are held `strong`ly
* You can use any object as a key provided it implements `hash` and `isEqual`. Consequences:
    * `NSString` is a good choice:
        * `hash` and `isEqual` is based on characters in string
    * `NSObject` is a bad choice:
        * pointer is the hash and == is the isEqual
        * any subclasses of NSObject I create are also poor
* Neither a key nor value can be `nil` - use `NSNull` instead
* Has methods to read from and write to disk as a plist file
* NSMutableDictionary
    * mutable version
    * create using alloc/init or one of the `-(id)dictionary ...` methods
    ```
    -(
    ```
* can loop through them with `for-in`
    ```
    for (id key in myDict) {
        id value = [myDict objectForKey:key]; // value
        id value = myDict[key] // same as above
    }
    ```
* can get all values as array and all keys as array


## Property list

"Property list" just means a "collection of collections". It is _any graph of objects_ containing only:

1. NSArray
2. NSDictionary
3. NSData
4. NSNumber
5. NSString
6. NSDate

(or mutable subclasses thereof)

The above are all collections. A property list is

Examples:

* array of strings
* array of arrays
* dictionary of arrays of strings

A dictionary is only a property list if both its keys and values are in the above list.
Note that "property list" is _not_ a type - it is just a phrase we use. It matters because the iOS SDK has a number of APIs that take "property lists" (the type is usually `id`)


### NSUserDefaults

* a dictionary that persists across application launches
* not a full on database
* only store small things like user preferences
* is a singleton
* even if you give it a mutable thing, you will get back an immutable version

```
[NSUserDefaults standardUserDefaults] // get access to the singleton instance
[[NSUserDefaults standardUserDefaults] synchronize] // always write changes to disk quickly
```


### NSRange

* is a C struct not a class
* used to specify sub ranges within strings, arrays etc.

```objc
typedef struct {
    NSInteger length;
    NSInteger location;
} NSRange;
```

* has the `NSNotFound` constant which you get back if you ask for a location
  that is somehow invalid.
* Notice that the location cannot be negative!

```objc
NSString *greeting = @"hello there";
NSString *fragment = @"hi";
NSRange r = [greeting rangeOfString:fragment];

// notice that we test **location** not the range itself
if (r.location == NSNotFound) {
    // failed to find the substring in the string
}
```

NSRangePointer is just `NSRange *`

Used by methods that take a reference to an `NSRange` as a parameter and will fill it in. Examples are the C functions

* `NSEqualRanges()`
* `NSMakeRange()`


## UIKit classes

### UIColor

* an object representing a color
* can be represented as RGB, HSL etc.
* can even be a pattern (UIImage) - when you draw with the color it will draw with the pattern
* can also have alpha
  ```objc
  UIColor *c = [otherColor colorWithAlphaComponent:0.3];
  ```
* there are some "standard" colors that have class methods:
  ```objc
  [UIColor greenColor];
  [UIColor redColor];
  [UIColor purpleColor];
  ```
* there are also some "system" colors that have class methods:
  ```objc
  [UIColor lightTextColor];
  ```

### UIFont

* fonts are very important in iOS

The prefered way of getting an UIFont for _user content_ (not buttons etc.) is
to ask for the _prefered font_ for a given _text style_.

```objc
UIFont *f = [UIFont preferredFontForTextStyle:UIFontTextStyleBody];
```

Examples of font styles (UIFontDescriptor):

* UIFontTextStyleBody
* UIFontTextStyleHeadline
* UIFontTextStyleCaption1
* UIFontTextStyleFootnote

There are also "system fonts" designed for use on the app chrome. In general you should never use these for user content.

```objc
+ (UIFont *)systemFontOfSize:(CGFloat)pointSize;
+ (UIFont *)boldSystemFontOfSize:(CGFloat)pointSize;
```

Aside: `CGFloat` is the float type from _Core Geometry_

### UIFontDescriptor
* attempts to put categories on fonts
* tries to map font metrics into stuff we care about as devs
* it categorizes by family, face, size etc.
* You can ask for fonts that have those attributes and get a "best match"
* Consequences: the best "match" for a request for "bold" might not be bold if the font does not include a bold weight


### Attributed Strings

The presentation of text depends on a number of things:

* font
* fill color
* underline
* outline
* stroke width

`NSAttributedString` brings all these together in an object that has a dictionary of attributes for _each character_.

* Note that it is **not** a subclass of `NSString`!
    * You cannot send it "string" messages.
    * it has the `string` method
    ```
    NSAttributedString *str = ...
    NSString substr = ...
    [str string]; // gives us back a real string
    NSRange r = [[str string] rangeOfString:substring];
    ```
    * the string you get back is high performance but volatile
    * If you want to keep this string around make a copy of it
* It is immutable
* Usage:
    ```objc
    // we pass in a range which will be filled in to tell us how many characters
    // have the same attributes as the dictionary we just got
    // it is ok to pass NULL for the range pointer if you don't care
    - (NSDictionary *)attributesAtIndex(NSUInteger)index effectiveRange:(NSRangePointer range);
    ```

`NSMutableAttributedString`

```
- (void)addAttributes(NSDictionary *)attributes range:(NSRange)range;
- (void)setAttributes(NSDictionary *)attributes range:(NSRange)range;
- (void)removeAttribute(NSString *)attributeName range:(NSRange)range;
```

How do we modify the characters (not attributes) of an `NSMutableAttributedString`?

We use `mutableString` message which gives back an instance of
`NSMutableString` which magically tracks the contents of our mutable attributed
string.

```objc
[mutAttStr mutableString]; // gives back a mutable string
```

Attributes of attributed strings

```objc
UIColor *transparentYellow = [[UIColor yellowColor] colorWithAlphaComponent:0.3];

@{
    NSFontAttributeName: [UIFont preferredFontForTextStyle:UIFontTextStyleHeadline],
    NSForegroundColorAttributeName: [UIColor greenColor],
    NSStrokeWidthAttributeName: @-5, // negative means "fill and stroke, positive => stroke only
    NSStrokeColorAttributeName: [UIColor redColor],
    NSUnderlineStyleAttributeName: @(NSUnderlineStyleSingle), // NSNumber that has an enum
    NSBackgroundColorAttributeName: transparentYellow
}
```

Places we can use attributed strings:

* UIButton
    * attributedTitle
* UILabel
    * attributedText (immutable property)
* UITextView

For example:

```objc
[someUIButton setAttributedTitle:(NSAttributedString *)title forState:...;
```

Aside: what do I need to do to make space in memory in objC?
    is declaring a primitive enough
    "objects" are on the heap so need free/malloc under the hood i.e. some flavour of alloc+init

### UITextView

* A bit like UILabel but is selectable, scrollable, editable
* Has an `NSTextStorage` property (a subclass of NSMutableAttributedString) that represents its contents
* Each character has a dictionary of attributes but you can set the font for
  the UITextView as a whole. THis just sets whatever font you provide as the
  font for each individual character
    * Be aware that since traits (bold, italic) are part of each characters
      font info you will overwrite them when you set the font for the whole
      UITextView
* UITextView can do advanced layout with TextKit (new in iOS 7) e.g. exclusion
  zones. See the docs for the NSTextContainer and NSLayoutManager for full
  details.
    * The LayoutManager is the thign that lays the glyphs of a font out on the
      screen

### View Controller life cycle

A sequence of messages is sent to views as they progress through their life.

* controller is initialized
* geometry (bounds) changed e.g. screen rotation
* was put on screen
* was taken off screen
* low memory situations


Storyboards do not generate code. Tutor explains it as we are editing instances of the view objects live in Xcode - these views are then serialized (think JSON, not code) to disk and deserialized when the app runs

Lifecycle of a ViewController

1. Creation of view controller instance
1. Outlets are set
`- (void)viewDidLoad` is called
    * outlets are set so we can work with the contents of the view e.g. buttons
    * this is the place to do initialization of the view controller
    * Note: it is called before the view appears on screen
    * => the bounds of the view are not set so you can't do any stuff that is to do with the shape of the view on screen
    * Is only called **once** in the lifecycle of the view controller
1. `- (void)viewWillAppear:(BOOL)animated` is called
    * this is called _every time_ your view is about to appear (`viewDidLoad` is only called once)
    * the boolean arg just tells you whether you are being animated onto the screen or not.
    * You have geometry information in here so you can do it here but this will not be called when the phone rotates so you might not want to put it here
        * there is a better place to do it
        * t
    * Things to do here:
        * initialization you need to do in response to data that might have
          chnaged while you were off screen
        * you probably want to do expensive work in here. In viewDidLoad you don't know that your view will _ever_ appear on screen
1. You appear on screen
1. `- (void)viewDidAppear:(BOOL)animated` is called
1. You are _about to_ go off-screen
1. `- (void)viewWillDisappear:(BOOL)animated` is called
    * Things you should do here:
    ```objc
    [super viewWillDisappear]; // let the superclass method have a go
    ```
    * Things you can do here:
    ```objc
    [self rememberScrollPosition];
    [self saveToPermenantStorage]; // <-- if time consuming, do in a thread
    ```
1. `- (void)viewDidDisappear:(BOOL)animated` is called



Alwasy let the inherited version of a lifecycle method have a chance to run using `[super viewDidLoad];`
```objc
- (void)viewDidLoad
- (void)viewDidReceiveMemoryWarning
```
