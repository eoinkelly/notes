# Cocoa Frameworks

* A framework is
    * a collection of compiled code and header files
    * like a DLL, it is a chunk of compiled code that can be invoked by your app at run-time
    * The linker takes care of making your code talke to the framework code
* You can put anything in a framework that you can put in an app bundle
* You acn store multiple versions of a framework in one _framework bundle_ (but
  it is not common.
* Ideally you want it so users of your framework just have to import one header
  file
* You can mark the headers in your bundle as public and private to control
  whether they get copied into the bundle


## The binary

* The framework binary is a `.dylib` file (similar to `.dll` on windows)
* The `Versions/A/FrameworkName` (in the example below) is a binary that can contain multiple
  architectures built-in. e.g.
    * i386
    * armv7
    * armv7s
    * x86_64
    * etc.
* You only need the framework binary at runtime (this is the only bit copied
  into your app's bundle). The headers are only needed by the compiler/linker.


## File structure

* This is created & managed by Xcode

```
/path/to/FrameworkName.framework
  FrameworkName -> ./Versions/A/FrameworkName
  Headers -> ./Versions/A/Headers/
  Resources/ -> ./Versions/A/Resources/
  Versions/
    current -> ./A
    A/
      Headers/
      Resources/
      DeprecatedHeaders/
      FrameworkName (a binary file)
```



Resources

* contains
  `Info.plist`
  a .bundle file


# What is a bundle file?


# Where to put frameworks

1. System accessible
    * `/Library/Frameworks`
    * requires root access to install them - rarely used
    * Mostly useful to Apple only

2. User accessible
    * `~/Library/Frameworks`
    * Mostly useful to Apple only

3. Application accessible
    * You bundle them wiht your app so it can always access them and doesn't
      depend on something else in your system

# An alternative to using a framework

* Just include the sourcecode of the framework in your app and compile it into
* your binary

# Important Frameworks

* Foundation
    * Contains most of the base `NS*` classes
* UIKIt
    * Contains most of the iOS UI classes

# Practical points of using frameworks

* Xcode caches frameworks in its derived data so you will have to clear this
  after upgrading a framework

# Questions

what is the diff between including a framework and just including a dylib?

what is the diff between a dylib and a .a file?

how do I replace a framework in an xcode project?
