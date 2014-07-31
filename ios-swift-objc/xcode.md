

A sample gitignore for xcode projects
https://gist.github.com/adamgit/3786883


xcuserdata dir

* stored in `MyExampleProject.xcodeproj/xcuserdata`
* saves
  * user state
  * what folders you have open
  * last file openeed
  * the UI state of Xcode
  * `schemes` are stored in here
* xcode will create it for you the first time you open the project

## Xcode scheme

* Only one scheme can be active at a time
* When you select an active scheme you also select a _run destination_
* A run destination = the hardware architecture of the build products
* An Xcode scheme defines 3 things
    1. _collection_ of targets to build,
    2. a configuration to use when building,
    3. a collection of tests to execute.
* They can be stored
    1. in a project: they are available to every project that includes this project
    2. in a workspace - they are only available within that workspace
    * from the scheme's POV it has a container

In Xcode UI you can configure schemes via

* `Product > Scheme` menu
* You can choose the scheme using the almost hidden menus beside the big play/stop
  buttons
    ```
    {play} {stop} {active scheme} > {active destination} > {active OS level}
    ```
* It seems all schemes contains the following {thingies???}
    * Build
    * Run
    * Test
    * Profile
    * Analyze
    * Archive

* schemes can be shared ???
