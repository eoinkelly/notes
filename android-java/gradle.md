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
