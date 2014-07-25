# Big Nerd Ranch guide to iOS programming

## Chapter 1

* It follows traditional MVC - every button, label etc. is a view
* `UIView` is the parent class.

UIButton < UIView

Q: what template do seasoned programmers use?

Cocoa touch = ?
Foundation framework = ?

### Xcode & Git

* Xcode creates an absraction over SVN and Git
* When you create a new app you can initialize a git repo
* creates a git repo but does not add or commit anything
* has pretty good git support for basic operations

#### Questions

* SHould `.xcuserstate` files be saved to git? Xcode seems to think so.
* xcode creates no .gitignore - what is a good one for xcode projects?

### Xcode files

If I create a project `Foo`

```
Foo.xcodeproj
Foo/
FooTests/
```

* creates a folder with same name as the project


In `project navigator`

There are 3 main levels of organisation

1. projects
2. groups
3. files

These do not correlate well with the filesystem

### Terminology: Group

* groups is the correct term for things with a yellow folder icon
* groups do not correlate to the filesystem *at all*

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

### Derived data ??

can be set at project or workspace level

### Snapshots ??

Snapshots are archives that include the current state of all project and
workspace settings and all document files in the project.

#### Questions

* how do snapshots and git work?
* are snapshots a way of overcoming oddness of git + xcodeproj files?


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

#### Questions

Q: How does this work in practice?
Q: What are best practices for managing files in Xcode?
