# Gradle

A gradle build is controlled by _more than one_ `build.gradle` file

* is a DSL built on top of Groovy
    * groovy is interpreted (interpreter runs from a .jar file)
    * implications:
        * your gradle files are not just static declarations - they are also runnable code
* does not do compiling/linking/packaging itself but directs other tools in how to do it

Alternatives

* Ant
* Maven
* make
* rake

~/.gradle/ has stuff

gradle seems to be started from a sh script that basically loads

> CLASSPATH=$APP_HOME/lib/gradle-launcher-2.2.1.jar

# For android

currently android studio comes with gradle 2.2.1
currently brew has 2.2.3

Seesm to be started by a wrapper script that is in the sdk/tools dir

Android studio comes iwth a copy of gradle

    /Applications/Android Studio.app/Contents/gradle/gradle-2.2.1

but there is an option to run it from a wrapper script

> the project-level Gradle file uses buildscript to define the Gradle
> repositories and dependencies. This allows different projects to use
> different Gradle versions. Supported repositories include JCenter, Maven
> Central, or Ivy.
