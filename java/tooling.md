# Tools for java stuff

## ProGuard

_proguard is not run by default in cordova projects_

* shrinks
    * removes unused code
* optimizes
    * removes unused code
* obfucates
    * renames classes, fields, methods with obscure names


* results in a smaller APK that is harder to reverse engineer
* integrates into the android build system
* only runs when you build in release mode
* ProGuard does not run in debug mode because it makes debugging harder.
* is optional but recommended
* is controlled by `proguard.cfg` in project root

* To enable proguard
    * set the `proguard.config` property in your `project.properties` file

```
// project.properties
proguard.config=proguard.cfg

// if not in project root
// proguard.config=/path/to/proguard.cfg
```

There are 3 ways of building an android project in release mode
Build in release mode using

1. `ant release`
    * Any build in _release mode_ will run proguard if it is set in `project.properties`
    * output files in `bin/proguard/`
2. Eclipse export wizard
    * Any build in _release mode_ will run proguard if it is set in `project.properties`
    * output files in `proguard/`
3. Using gradle (in AndroidStudio)
    * To use ProGuard with gradle you need to add it to your `gradle.build` file's build types


ProGuard outputs 4 files when it runs:

* dump.txt
    * describes the internal class structure of the apk
* mapping.txt
    * basically a sourcemap of the orignal code to new code
    * you need this if you want to decode stack traces you get from customers
      using the obfuscated build.
* seeds.txt
    * classes and members that are not obfuscated
* usage.txt
    * list the code that was stripped from the apk

Where they are put depends on build system

## ant

## gradle

*  Run the `gradle` command from a dir that has a `build.gradle` file

* gradle tasks <---> ant targets

## jarsigner

# zipalign
