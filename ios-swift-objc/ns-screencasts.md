

# objc_msgSend

```objc
[user logoutNow]

// becomes

objc_msgSend(user, @selector(logoutNow), nil);
//          reciever, selector, args

```

# C strings vs NSString

```objc
"hello"  // creates a C string
@"hello" // creates an instance of NSString
```



# Manual memory management

* ObjC on desktop has GC but not on iPhone
* ARC is only available on iOS 5+
* This is how to manually manage release and retain

```objc
// creating an Objective-C object has two steps
NSMutableArray *ary = [[NSMutableArray alloc] init];

[ary release]; // drecrement the retain count
```

* Never call `dealloc` yourself

1. Methods named `init`, `new`, `copy` the returned object has a retain count of
   and you must release the object when you are finished with it.
      * Any object returned from any other method is assumed to be autorelease.
2. If you recieve an object and intend to hold on to it you need to `retain` it.


How to know when you need to release?

* Did I create this object? THen YES
* Did I call retain on this object? Then YES
* Otherwise NO.

```objc
// Consider the factory method:
- (UILabel *)labelWithText: (NSString *)text
{
  UILabel *label = [[UILable alloc] init];
  [label setText:text]

  // The problem:
  // * we have 'label' which has retain count of 1
  // * clients of this method will not know that they need to call release on it
  // * if we don't release it before we return it will never get destroyed
  return label;


  // Put label in the autorelease pool that the runtime  managers
  // DUring the next time through the runloop the autorelease pool is drained.
  return [label autorelease];
}


// Here we have to call retain to keep the reference to our label.
// We now own the label
UILabel *myLabel = [[self labelWithText:@"my label"] retain];
```

* Objective C has a run loop


# Format of a header file

```objc

#import <stuff>

// Everything goes between @interface .. @end
@interface ClassName : Parent

// There are 4 sections in here:
// 1. instance variables
// 2. properties
// 3. class method declarations
// 4 instance method declarations

// Section 1 (Instance variables) is surrounded by {}
{
  NSString *_foo;
  // notice we _ the variable to indicate it is for internal class use

}

// Section 2 (properties)

// Section 3 (class method declarations)

// Section 4 (Instance method declarations)

- (NSString *)name; // getter for name
- (void)setName: (NSString *)n;
@end

```

```obc
#import "ourclassname"

// The implementation file repeats the @implementation stuff
@implementation ClassName : Parent

// There are ??? sections in here

// 1. Instance method implementations

// Getters and setters the hard way:

- (NSString *)name {
  return _name;
}

// Objective C setters are pretty complex (compared to other langs) because of
// the memory management
- (void)setName: (NSString *)newName {
  [_name release]; // make sure old value gets releaseed
  // this would complicate memory management as _name would be another reference
  // to whatever newName is
  // _name = newName;
  _name = [newName copy]; // convention is to use copy for string setters
}

- (void)dealloc {
  // release our instance variables
  [_name release];

  [super dealloc]; // should be last line
}

// 2. Class method implementions

@end
```

# @synthesise

```objc
@synthesise name = _name;
// reads as:
// "Make accessor methods (which ones are controlled by declaration of `name`
// and use `_name` as the instance variable name (which you will refer to from
// other methods in the class)"
```

Q: can you get at instance variables wihout a getter/setter in objc?
