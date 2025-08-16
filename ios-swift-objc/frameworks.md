# Units of code

## `.tbd` binary file

https://forums.developer.apple.com/message/9176#9176

> .tbd files are new "text-based stub libraries", that provide a much more
> compact version of the stub libraries for use in the SDK, and help to
> significantly reduce its download size.

- they seem to be a YAMLish format
- they reference a dylib file

Example of a `.tbd` file

```
--- !tapi-tbd-v2
archs:           [ armv7, armv7s, arm64 ]
uuids:           [ 'armv7: C2DC3FAB-8AE4-3C0C-AC50-D9C9A3F5D042', 'armv7s: 741AA09D-2104-30D0-8998-D6390FC52D94',
                   'arm64: F1A568E3-93D5-31F6-AF5E-1EDB816CDAB2' ]
platform:        ios
install-name:    /usr/lib/libsqlite3.dylib
current-version: 254.6
compatibility-version: 9
objc-constraint: none
exports:
  - archs:           [ arm64 ]
    symbols:         [ _sqlite3one ]
  - archs:           [ armv7, armv7s, arm64 ]
    symbols:         [ __sqlite3_lockstate, __sqlite3_purgeEligiblePagerCacheMemory,
...
```

## `.dylib` binary file

- The framework binary is a `.dylib` file (similar to `.dll` on windows)
- The `Versions/A/FrameworkName` (in the example below) is a binary that can
  contain multiple architectures built-in. e.g.
    - i386
    - armv7
    - armv7s
    - x86_64
    - etc.
- You only need the framework binary at runtime (this is the only bit copied
  into your app's bundle). The headers are only needed by the compiler/linker.

## Framework bundle file structure

- This is created & managed by Xcode

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
          Info.plist
          (sometimes other stuff too e.g. image assets)
      DeprecatedHeaders/
      FrameworkName (a binary file)
```

## Where to put frameworks

1. System accessible
    - `/Library/Frameworks`
    - requires root access to install them - rarely used
    - Mostly useful to Apple only

2. User accessible
    - `~/Library/Frameworks`
    - Mostly useful to Apple only

3. Application accessible
    - You bundle them with your app so it can always access them and doesn't
      depend on something else in your system

## An alternative to using a framework

- Just include the sourcecode of the framework in your app and compile it into
  your binary

## Important Frameworks

- Foundation
    - Contains most of the base `NS*` classes
- UIKIt
    - Contains most of the iOS UI classes

## what is the diff between a dylib and a .a file?

- The object file format used in OS X is Mach-O

- Modules
    - the result of compiling a single C file e.g. `foo.c` becomes `foo.o`
    - the smallest unit of machine code + data that can be linked independently
      with other code
    - manipulated by `ld` e.g. `ld -r -o output.o in1.o in2.o in3.o`
- Static library (aka static archive) file
    - some modules can be grouped into a static archive
    - is basically a group of modules with a table of contents
    - has a `.a` extension by convention e.g. `mylib.a`
    - built by `libtool` , manipulated by `ar`
    - libtool
        - e.g. `libtool -static foo.o bar.o -o libstuff.a`
        - libtool can also create dynamic libs
    - ar
        - `ar -t ./path/to/mylib.a` lists the `.o` files in the archive
    - lumps of code that the static linker can add to your binary at _build
      time_
    - used for smaller lumps of code (presumably because it adds to your binary
      size)
- Dynamic library (aka dylib or framework)
    - also called _shared libraries_ or _dylibs_
    - referenced by your code
    - loaded _at runtime_ by the dynamic linker
    - used for larger lumps of code
- Kernel extensions
    - packaged similarly to bundles
    - loaded into Kernel memory so built quite differently
- Frameworks
    - a shared libary along with resources like localised strings, headers,
      documentation
    - From a tools perspective, a framework is a shared library whose install
      name ends in the form
      `frameworkName.framework/Versions/versionName/frameworkName` or the form
      `frameworkName.framework/frameworkName`.
    - e.g. UIKit, Foundation
    - You can put anything in a framework that you can put in an _application
      bundle_ e.g. image assets.
    - You can store multiple versions of a framework in one _framework bundle_
      `.framework`
    - Ideally you want it so users of your framework just have to import one
      header file
    - You can mark the headers in your framework bundle as public and private to
      control whether they get copied into the bundle
- Umbrella frameworks
    - collections of frameworks
    - e.g. Cocoa
- Bundles
    - executable files that your program can load _at runtime_ using dynamic
      linking
    - Bundles are plug-ins, dylibs are shared libraries.
    - bundle's cannot be statically linked against, they must be loaded by user
      code.
    - Has 2 meanings on OS X
        1. the actual object file containing the code
        2. the directory containing the object file and associated resources
    - Applications in OSX are packaged as bundles (the second meaning)
        - They are displayed in Finder as a single file (they are also know as
          _Application Packages_)

More info

- https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachOTopics/0-Introduction/introduction.html#//apple_ref/doc/uid/TP40001827-SW1

## What is the diff between including a framework and just including a dylib?

- A framework _is_ a shared library (dylib) plus any related resources.
    - Including a framework _is_ including a dylib.

## How do I replace a framework in an Xcode project?

From my first attempt:

- Delete it via Xcode and then add back in the new version via the list of
  frameworks on the project info pane - this seems to work.
- There does not seem to be a way to do it in the project navigator
- Xcode caches frameworks in its derived data so you will have to clear this
  after upgrading a framework
