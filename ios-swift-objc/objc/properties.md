# Instance variables

Objects contain _instance variables_

- each instance variable
    - is declared (so compiler allocates space on heap) in the `@interface`
    - is initialized (given a value) by some method in the `@implemnetation`
      block
- they are accessbile within the object but not outside
- to make them accessible from outside we need to create getter and setter
  methods

```objc
@interface BlogPost : NSObject
// instance variables declared between curlies
{
    NSString *foo; // an instance variable
}
@end

@implemnetation

@end
```

`@synthesize` and `@property` are sugar for creating getters and setters

# @property

- Creates a getter and a setter method
- Creates an instance variable with an underscore prefix

Q: is it true that @synthesize is not required any more?

Property _attributes_ (<-- note the terminology)

```objc
@property (attributes, are, comma, separated) Type *iVarName;
```

1. strong|weak|copy|assign
    - assign = default for non pointers (non objects e.g. int)
    - strong = create a instance variable with a strong pointer (default for
      objects)
    - weak = create an instance variable with a weak pointer
    - copy = copy the value in the setter so we work with our own copy of the
      data
        - NOTE: always use this for instances of `NSString` or `NSArray` (which
          might be an `NSMutableArray`) as you may complicate ownership if you
          don't copy.
1. atomic|nonatomic
    - atomic = ??? (default)
        - generated getter and setter has a lot more wrapping code to make it
          thread safe
    - nonatomic = suitable for single theaded apps
        - Question: Why is NS_NONATOMIC_IOSONLY (which seems to typedef to
          `nonatomic`) a thing?
1. readwrite|readonly
    - readwrite = make a getter and setter (default)
    - readonly = make only a getter
1. getter=someName
1. setter=someName

# @synthesize

- It is not possible to `@synthesize` without declaring a `@property` first!
- @synthesize implements a default getter and setter method
- it uses an instance variable of the _same_ name and type of the property.
    - if one does not exist it creates it.
    - note that when you don't use `@synthesize` that `@property` on its own
      will create the instance variable with an underscore prefix e.g. property
      `foo` will create instance variable `_foo`.

- You can specify the name of the internal instance variable
    - use an `_foo` naming scheme for private variables

```objc
@synthesize date; // = date;

// specify the name of the instance variable, does not need to be declared
@synthesize date = _when;
```
