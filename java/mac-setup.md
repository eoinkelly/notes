
Java 7 does not support MacOS 10.10

Install JDK http://www.oracle.com/technetwork/java/javase/downloads/index.html
Uninstall JDK (manual rm -rf ing) http://stackoverflow.com/a/23092014

* Java Runtime Environment (JRE)
    * what end users need to run java apps
* Server JRE
    * JRE + JVM monitoring tools for server admins
* JDK
    * JRE + JVM Monitoring + stuff devs need

# Javas on my system

 1. built-in jdk stuff that comes with macosx
    * in `/Library/Java/JavaVirtualMachines`

# Android SDKs on my system

1. ~/adt-bundle-mac-x86_64-20140702
2. /usr/local/android-dev-tools

# Other Java tools on my system

* ant from brew
* gradle from brew


Categories of tools that I have:

* command line e.g. adb, cordova
    * currently using the SDK from /usr/local/android-dev-tools (see paths below)
    * currently using built-in OSX java, javac
* eclipse
    * currently using built-in OSX java, javac
    * is using SDK from `/Users/eoinkelly/adt-bundle-mac-x86_64-20140702/sdk`
    * I _think_ eclipes does not use ANDROID_HOME if the SDK location is set in
      its preferences.
* android studio
    * had to install slightly older version of java (http://support.apple.com/kb/DL1572) to get it to work on yosemite
    * using `/Applications/Android Studio.app/sdk` as SDK location


# Environment setup for java dev

~/.zshrc
    export JAVA_HOME=$(/usr/libexec/java_home)
    export ANDROID_HOME=/usr/local/android-dev-tools/sdk

~/.zprofile
    export GRADLE_HOME="/usr/local/Cellar/gradle/2.0"
    export PATH="/usr/local/android-dev-tools/sdk/platform-tools:$PATH"
    export PATH="/usr/local/android-dev-tools/sdk/tools:$PATH"


Aside:

    # add to /etc/launchd.conf
    setenv ANDROID_HOME /path/to/android-sdk

seems to be how you set environment variables for non terminal access
??? but that file does not exist by default on Yosemite at least ???

# Config places

    ~/.android/
    ~/.gradle/
    ~/.m2/ (my maven repository)


My current version is java version "1.6.0_51"
    this seems to be yosemite java
