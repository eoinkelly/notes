## Xcode Metadata

Sources

- https://developer.apple.com/library/ios/featuredarticles/XcodeConcepts/Concept-Workspace.html
- WWDC 2012 video https://developer.apple.com/videos/wwdc/2012/ "Working with
  Schemes and Projects in Xcode"

# 5 concepts

There are 5 core concepts to understand:

1. Workspaces
2. Projects
    - Build configurations
3. Targets
    - Build settings
    - Build phases
    - Build rules
4. Schemes
5. Run destinations

# 1. Workspaces

- You always have a workspace in Xcode - even if you have just one project Xcode
  will create an implicit workspace for you.
    - It will stash a workspace "document" (acutally a dir) inside the project
      dir `FooProj/FooProj.xcodeproj/project.xcworkspace/`
    - So projects can contain workspaces and workspaces can contain projects
- Groups together projects
    - Contains references to projects and other files
- Allows targets to have their implicit dependencies from other projects found.
    - Will automatically build targets that are depended on by other targets in
      the same workspace
        - QUESTION: how do i make a target depend on another target?
            - import a header ???
            - To add a dependency directly to a target go to the Target
              "settings" > "Build phases" and add it. This might not be the
              idiomatic way to do it tho - maybe these get added by linking to
              frameworks ???
- Provides:
    1. a unique location for build products
    2. state such as:
        - build logs
        - "symbol index" ???
        - saved window state

A workspace looks like a file in finder but is a dir. As an example:

```
➜  PodsPlay git:(master) ✗ tree PodsPlay.xcworkspace
PodsPlay.xcworkspace
├── contents.xcworkspacedata
└── xcuserdata
    └── eoinkelly.xcuserdatad
        └── UserInterfaceState.xcuserstate

2 directories, 2 files

contents.xcworkspacedata - an XML file that contains refs to `SomeName.xcodeproj` dirs
UserInterfaceState.xcuserstate - an apple binary property list
```

To view contents of a binary plist file:

```
# plutil -p SomeBinaryPlistFile | less
$ plutil -p UserInterfaceState.xcuserstate | less
```

- xcuserdata dir
    - stored in `MyExampleProject.xcodeproj/xcuserdata`
    - saves
    - user state
    - what folders you have open
    - last file openeed
    - the UI state of Xcode
    - `schemes` are stored in here
    - xcode will create it for you the first time you open the project

Every workspace has its own derived data dir (not stored under under your Xcode
project dir) Multiple copies of the same workspace will use differen derived
data dirs.

# 2. Projects

A project contains 4 kinds of thing:

1. references to source files
2. targets which process source files build products
    - a target is a recipe for turning 1+ source files into a "product" e.g.
      static lib `.a` or dynamic lib `.dylib`.
3. schemes which build targets and perform actions on their products
    - a scheme is a thing which runs "targets" and then performs actions on the
      "products" they create.
4. build configurations which select variants of target build settings
    - examples of build configurations: Debug, Release
    - you can create others
    - you have a collection of "build settings" for your project that say how it
      should be built e.g. optimisation level for the compiler. You want to vary
      these depending on what kind of build you are making - build
      configurations let you do that.
    - build configurations are like global flags that many targets can adjust
      their build settings for.
    - QUESTION: is a build configuration a separate thing on disk (either
      file(s) or part of file(s))?

On disk it looks like:

```sh
➜  Katas git:(master) ✗ tree Katas.xcodeproj
Katas.xcodeproj
├── project.pbxproj
├── project.xcworkspace
│   ├── contents.xcworkspacedata
│   └── xcuserdata
│       └── eoinkelly.xcuserdatad
│           └── UserInterfaceState.xcuserstate
└── xcuserdata
    └── eoinkelly.xcuserdatad
        ├── xcdebugger
        │   └── Breakpoints_v2.xcbkptlist
        └── xcschemes
            ├── Katas.xcscheme
            └── xcschememanagement.plist

7 directories, 6 files

```

notice the embedded workspace with same set of files as an explicitly created
one

There is also a top-level `xcsharedata` within the project if you have project
data you want to share between all users of the project e.g. schemes.

The Xcode project file (`project.pbxproj`) is a PList file and can be serialized
to disk in 2 ways:

1. (OpenStep) ASCII format (looks like a more free-form JSON)
    - deprecated but still used by Xcode itself
    - consequence: sometimes your entire project file will change if you have
      cocoapods installed as it can only use the XML format and Xcode used the
      ASCII format.
2. XML format
    - Used by Cocoapods

It contains (from inspection, not complete list):

- references to each source file and resource
- info about what groups they are in
- a serialized version of all the build settings for the project

`xcuserdata` seems to be also available in a workspace but this one is tied to
my login also seems to contain schemes and a list of the breakpoints I have
created

Note: Xcode uses only its metadata files to find source files and resources on
disk - it does not care about the contents of your project directory on the
filesystem - only those files that have been "imported" into it.

Terminology

Select the "Project" from the "Project navigator" - this presents you with the
"source list" column the right-most column will either be "project editor" or
"target editor" depending on what you choose.

# 3. Targets

A target is a recipe for turning 1+ source files into a _single_ "product" e.g.
static lib `.a` or dynamic lib `.dylib`. It also contains info about what other
targets need to be built before this one can be.

Contains:

- References to 1+ source files in the project
- The build phases. Typically:
    1. Target dependencies
        - Lets you add dependencies - note that the UI here shows implicit
          dependencies here too so if your target depends on A which depends on
          B and C you should A, B,C listed here.
    2. Check Pods manifest.lock (if using cocoapods)
    3. Compile Sources
        - lets you set per file compiler flags
    4. Link binary with libraries
    5. Copy bundle resources
    6. Copy pods resources (if using cocoapods)
    7. Copy headers
        - Only available when building a framework
        - Lets you choose which headers are "public", "private" or "project"
            - "project" means the header is available to only this project and
              will not be copied into the output.
            - QUESTION: what does "public", "private" mean?
- One of these is "Compile sources"
    - A set of build rules tells this phase how to handle each file-type
    - NB: The "build rules" are instructions to the "Compile sources" phase even
      though that relationship is not clear in the Xcode UI
- Build settings which control how the rules are applied
- A set of 0+ other targets which must be built before this "product" can be
  built.

# 4. Schemes

- A scheme is an object
- A scheme is an XML file with extension `.xcscheme`
    - Lives in the `.xcodeproj/` dir in either `xcuserdata` or `xcsharedata`
      e.g.
        - `PodsPlay.xcodeproj/xcuserdata/eoinkelly.xcuserdatad/xcschemes/PodsPlay.xcscheme`
        - `Foo.xcodeproj/xcsharedata/xcschemes/Bar.xcscheme`
    - It seems that the scheme files live in the project not the workspace
- It is a set of instructions that contains the instructions for building 1+
  targets and then performing actions on the resulting products

- schemes are created automatically for you when you
    1. create a new target or project
    2. open a project or workspace for the first time these are created "per
       user" i.e. they are stored in the per-user data location of the project
       (path???) but if you tick the "shared" box in the schemes management page
       the scheme is moved to the shared data location of the project
       `Foo.xcodeproj/xcsharedata/xcschemes/Bar.xcscheme`
- schemes are not added to source control by default

You select one of the actions to perform and then the scheme will build a
particular set of targets in a particular way and then run the action. Note that
the choice of action chooses what targets get built and how they get built. The
actions are:

#### Build

- Build is not really an action but is a step that always has to happen before
  each action. The action specifies what kind of build will happen.
- Confusingly it is listed in the UI of the scheme editor in the same way that
  the actions are.
- In this UI Sheet you choose what targets will be built for each action. You do
  not choose the build configuraiton here as each action will want to choose
  that.
- The default build command (cmd-b) builds for the run action.

#### The 5 scheme actions

1. Run
    - Exposed as button in UI
2. Test
    - Exposed as button in UI
3. Profile
    - Exposed as button in UI
    - performance profiling
4. Analyze
    - Exposed as button in UI
5. Archive
    - Archive the project for distribution

When creating a new scheme I am asked to choose a single target - can add more
later Each scheme has a section for each of the 5 actions

When I am doing things in Xcode I am alwasy doing them within the context of a
particular scheme i.e.

when I do a build with cmd-b I am running the build part of the 'run' action of
the current scheme when I run tests I am running the test action of the current
scheme

I should think of the little "run" button as "Perform the run action (which will
trigger a build) of the current scheme.

- Only one scheme can be active at a time
- When you select an active scheme you also select a _run destination_
- Each scheme has a container - they can be stored
    1. in a project: they are available to every project that includes this
       project
    2. in a workspace - they are only available within that workspace
- schemes can be shared somehow ???

In Xcode UI you can configure schemes via

- `Product > Scheme` menu
- You can choose the scheme using the almost hidden menus beside the big
  play/stop buttons
    ```
    {play} {stop} {current scheme} > {current run destination}
    ```

# 5. Run destinations

- When you select an active scheme you also select a _run destination_
- A run destination = the hardware architecture of the build products
- Tells Xcode _where_ you want to perform the scheme action
- A run destination is a simulator or real device

# Tasks

## How to clean intermediate files in Xcode

Cleaning a project deleted the derived data.

- Product menu > Clean
- `cmd+shift+k`

Cleaning the build folder deletes the folder that the derived data is stored in.

- (hold option key) Product menu > Clean build folder
