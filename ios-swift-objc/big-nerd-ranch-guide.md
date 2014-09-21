# Big Nerd Ranch guide to iOS programming

## Introduction

* Class names start with Uppercase
* Method names start with lowercase
* methods and functions are different things in ObjC
    * method:
        * attached to a class or instance
        * they are not invoked directly - they are invoked by asking the
          class|instance to perform them via the `[receiver methodName]` syntax.
    * function:
        * plain old C function
        * applied by appending a parens wrapped list of arguments to the name `funcName(arg1, arg2)`

* ObjC started as a preprocessor for C so all ObjC stuff can be desugared to C

## Chapter 1

* It follows traditional MVC - every button, label etc. is a view
* `UIView` is the parent class of views e.g. `UIButton < UIView`

QUESTION: what template, if any do seasoned programmers use?

* Cocoa touch
    * the UI "umbrella framework" for iOS
    * it contains Foundation, UIKit, GameKit, MapKit etc.

* Foundation framework
    * A framework is Apple's dynamically linked library file  and enough headers to
      link against it
    * Foundation framework is a framework with many basic classes and is shared
      between Mac and iOS

### Xcode & Git

* Xcode creates an absraction over SVN and Git
* When you create a new app you can initialize a git repo
* creates a git repo but does not add or commit anything
* has pretty good git support for basic operations

Meanings of the annotations beside files in the project navigator:

Badge     SCM status
--------|---------------
M       | Locally modified
U       | Updated in repository
A       | Locally added
D       | Locally deleted
I       | Ignored
R       | Replaced in the repository
\-       | The contents of the folder have mixed status; display the contents to see individual status
?       | Not under source control


QUESTION: Should `.xcuserstate` files be saved to git? Xcode seems to think so.

QUESTION: xcode creates no .gitignore - what is a good one for xcode projects?

### Your project on the filesystem

If I create a project called `Foo` the Xcode creates 3 folders:

1. `Foo/Foo.xcodeproj`
    * (appears as a bundle in Finder)
    * stores Xcode metadata etc.
2. `Foo/Foo/`
    * application code goes in here
    * has a plist
        * TODO: what is the signifcance of this?
3. `Foo/FooTests/`
    * tests go in here
    * has a plist

## Project Navigator

In `project navigator` there are 3 main levels of organisation

1. projects
2. groups
3. files

These do not correlate well with the filesystem

### Terminology: Group

* groups is the correct term for things with a yellow folder icon
* groups do not correlate to the filesystem **at all**

### Terminology: Container

* A `container` is a role that a `project`, `workspace` or `group` can play for
  another project.
* It has no correlation with the filesystem.

### Terminolgy: Workspaces

* are for multi-product development
* are a common container for several related projects
* examples
    * if you have a project that depends on some other projects being built
      first you can put them all in a workspace and setup build rules about what
      order they are built in.
* To add a project to a workspace you must add the `project package`

### Terminology: The project package

A `project package` is a directory that the Finder displays as a file with a
`.xcodeproj` extension. It contains information about the project, such as
references to the files that are part of the project, the project's groups,
build settings, and target definitions.

### Derived data

* is where Xcode caches build artefacts etc.
* you can clean it out from the Organiser window
* can be set at project or workspace level

### Snapshots

Snapshots are archives that include the current state of all project and
workspace settings and all document files in the project.

* comparing snapshots is not particularly easy (although it is possible)
* they work by storing your source in a private git repo
* they are a kind of wy of having xcode do automatic commits at certain points
* for you e.g. after a successful build

### Auto-save

* Xcode autosaves when you build, commit, close the project or create a snapshot

QUESTION: snapshots seem to be a legacy attempt at some sort of SCM - can I ignore them?

QUESTION: how do snapshots and git work?

QUESTION: are snapshots a way of overcoming oddness of git + xcodeproj files?


### Adding existing files & folders to a project

Adding files & folders to Xcode does not move them on disk (unless you tick the
'copy' box). It just gives xcode a reference to them on your filesystem. There
are two kinds of reference you can create:

1. Folder reference
2. Group

* `folder reference` is a reference in the project navigator to a real folder on the filesystem.
* it shows up as a blue folder in project navigator (groups show up as yellow folders)

What is the differences between a `Group` and a `Folder Reference`?

* Group
    * Yellow folder icon
    * Does not appear on the filesystem at all
    * A totally internal to Xcode way of organising files
* Folder Reference
    * Blue folder icon
    * Live updates as the filesystem updates

When you are importing files you have to choose what should happen to any
filesystem folders in the thing you pick

1. make a new group in the project navigator based on the filesystem folder
   heirarchy
2. make a new `folder reference` in the project navigator for any added filesystem
   folder

You can also choose to include those files in any of the available builds - this
means they will be copied into the resulting `.app` directory.

It seems in Xcode you  have to manage your project navigator view and your
filesystem separately.


### Building Interfaces

Xcode GUI editor is an **object editor**

* saves stuff in `xib` files (XML docs)
* which are compiled into `nib` files during build
* the `nib` files are put in the bundle and loaded at runtime by the app
* can have many `xib` files per project (each one compiled to a `nib`)

### Terminology: Bundle

* Used on both Mac and iOS
* A directory containing the application executable and and any resources it
  needs

### Storyboard anatomy

* storyboard seems to be a visual representation of the controller and view objects in the app
* stores its data as XML (how git friendly is this?)

Scene

```
controller = a reference to (a?/the?) controller in the app
    View = an instance of UIView
    Top layout guide =
    Bottom layout guide =
first responder
    a relic from desktop - can ignore.
exit = ?
```

## MVC in Cocoa

Models usually use standard collection classes

* NSArray
* NSDictionary
* NSSet

Standard value types

* NSString
* NSDate
* NSNumber

* Contoller classes names end in `Controller`
* By default Xcode creates a `AppDelegate` controller and a `ViewController` when you create a project

AppDelegate is a **controller** - I guess it is similar to ApplicationController in rails

## Wiring views and controllers

The view talks with the controller - it does **not** talk to models!

### View -> Controller messages

Examples
    * I was clicked
    * I got new text entered

How to do the wiring

1. Declare and implement the controller method that the view should invoke in code
2. Wire it up to some action on the view visually

* IBAction
    * This is a type returned by methods that are triggered by view elements
    * kind of says "make this method visible in the XIB as a target for a view"


### Controller -> View messages

Examples:

* Change your text
* Show or hide yourself

* IBOutlet
    * defines an "outlet" for the controller
    * Decorate a pointer to a view element with this and I think you can do the visual wiring up in Xcode ???
    * the `IBOutlet`s in the controller are its references to views

Outlet = a pointer posessed by a controller that pointst to a view object

How to do the wiring:

1. Create a pointer to the appropriate UIView in the controller and decorate it as an "outlet"
2. Wire it up visually in the XIB
    * right-click on controller in storyboard, then drag from the outlet to the UIView you want it to point to.

### App delegate

* Every iOS app has one.
* it is a controller
* it is the **primary controller** of the app

### App entry

* Enters through main() just like an ordinary C app

## Keyboard shortcuts

* cmd+1 -> cmd+7 goes to each navigator in turn
* cmd+r = build

## lproj files

* Used for internationalisation
* Xcode has the notion of a `Base` localisation that you build your others on top of
* They are the folders on the filesystem that contain the localised versions of
  string files for xcode
* The UI displays each file in its context e.g. storybard stirngs file will be
  shown under the storyboard but on disk they are in teh `LANG_CODE.lproj` dirs

## Asset catalog

In Xcode:

```
`Images.xcassets`
    AppIcon
    LaunchImage
```

is actually on the filesystem as:

```
Images.xcassets
    AppIcon.appiconset/
      Contents.json
    LaunchImage.launchimage/
      Contents.json
```

* You still have to import the images into your project - the asset catalog just
  provides the link between the image and what is is used for in code (rather
  than having the image file name do that)

Advantages:

* lets you refer to the images from code using the catalog name (not file path)
    * UIImage:imageNamed:
* lets you avoid the `foo@2x.png` naming convention - you can name your images
  anything you want
* on iOS 7+ deployment targets XCode compiles the asset catalogs into a runtime
  binary format that makes it faster to load.

## PCH files

* Called the `prefix header`
* It is *automatically included in every source file* in the project without the
* use of compiler directives.
* They are _usually_ pre-compiled and cached to speed up build times.
    * They only have to be parsed once by the compiler
* they _can_ be used for project wide `#define` but those are a bit of a code
  smell.
* You should only `#include` headers in here that change rarely - otherwise the
  caching will be a net negative
* Downsides
    * they create a hidden dependency between your Foo.m|h files and the prefix
      header which means your source files can't be shared with other projects
      without it.
* Xcode 6 does not seem to make a PCH for new projects

* Opinion: http://qualitycoding.org/precompiled-headers/

## plist files

* contain the **runtime** settings for the app
* is an XML file.
* has a fancy editor in Xcode
* is copied into the bundle
* named `ProjectName-Info.plist`
* iOS asks this file about where to find the icons for the project

## Launch images

* BNR recommends that you make them look like the first screen of your app just without content
    * a splashscreen would be jarring if it only appeared for a second
* In Xcode 6 these seem to be actual storyboards rather than images

## Questions

Q: can you get at instance variables wihout a getter/setter in objc?

Q: what does @selector do?

Q: What are best practices for managing files in Xcode?

## Is ObjC and whitespace

In general ObjC seems to only require whitespace if there is no other character
that can tell it where one token stops and another ends. It does not seem to care how much whitespace you put in or what kind it is (spaces, tabs, newlines)

It does not care about whitespace in method signatures

```objc
// This is fine ...
-(IBAction)changeLabel:(id)sender{
    NSLog(@"you clicked!") ;
}

// as is this ...
 - ( IBAction ) changeLabel : ( id ) sender {
    NSLog(@"you clicked!") ;
}

// as is crazy shit like
 - (
    IBAction
    )
changeLabel
:
(
 id
 ) sender{
    NSLog(@"you clicked!") ;
}
```

It does not care about whitespace between a method name and the opening paren

```objc
// This is fine
NSLog(@"you clicked!") ;

// as is this
NSLog (@"you clicked!") ;

// as is this
NSLog
(@"you clicked!") ;
```

## The 'id' type

* `void *` means "a reference to some random chunk o' memory with untyped/unknown contents"
* `id` means "a reference to some random Objective-C object of unknown class"

* In GC only or "GC supported modes (of waht?) the compiler will emit write barriers around `id`
* Never use `void *` and use `id` as little as you can
* An `NSObject *` is preferable to `id` because the compiler can better validate it.

# Chapter 2 - Objective C

The method names in a class must be unique - there is no overloading based on parameter type. This does not mean that methods cannot share some labels e.g.

```objc
// this is ok
- (void) doThingTo: (NSString *)stuff {
}
- (void) doThingTo: (NSString *)stuff with: (NSString *)otherThing {
}
```

## nil

* The zero pointer
* Java: `null`
* C: `NULL`
* Used to represent the absence of an object
* is falsy when used in boolean tests

You can send any message to `nil` in ObjC and nothing will happen - opposite of Ruby!

## Destroying objects

Set the pointer that points at it to `nil`

TODO: add release retain to this story
