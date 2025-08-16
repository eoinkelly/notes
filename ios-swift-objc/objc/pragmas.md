# Pragmas

The pragma mark lets Xcode understand your source better - it does not change
runtime behaviour.

## #pragma mark

- pragma marks that begin with `-` have a divider line preceeding them when
  xcode is showing the dropdown list of methods in a class

It is a good idea to group the methods that implement a protocol together under
a `pragma mark` that names the prototocol

```objc

#pragma mark - SomeFooProtocol

- (void) partOfFoo { ... }
- (void) anotherPartOfFoo { ... }
```

You can also use pragma to temporaily disable certain compiler warnings - this
is a feature of C family compilers like clang.

```objc
// start an area of code where you are ignoring certain named warnings
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Warc-retain-cycles"


    // Do stuff that you know is ok but compiler has flagged false positive
    self.completionBlock = ^ {
        ...
    };

#pragma clang diagnostic pop
// stop ignoring the warnings again
```
