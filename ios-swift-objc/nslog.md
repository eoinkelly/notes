# NSLog

Usage examples of `NSLog`

```objc
// It is handy to box up primitives so that you don't have to use the exact
// format specifier
NSLog(@"number: %@", @(myNumber));

// Get the currently executing function
NSLog( @"calling: %s", __PRETTY_FUNCTION__ );

// Get a stack trace
NSLog(@"%@", [NSThread callStackSymbols]);
```


Boxing primitives in ObjC

# `#DEBUG` preprocessor macro

Xcode defines this to be 1 for debug builds and 0 for release builds so we can use it to wrap code that we only want to run in development

```objc

#if DEBUG
    NSLog(...
#endif
```
