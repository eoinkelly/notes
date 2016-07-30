# Command line cheat sheet

```
$ tree .
.
├── bin
│   └── info
│       └── eoinkelly
│           └── HelloWorldApp.class
└── src
    └── info
        └── eoinkelly
            └── HelloWorldApp.java

6 directories, 2 files

# compile all files in the package
$ javac -d bin src/info/eoinkelly/*.java

# run the entrypoint class (which must have a main method)
$ java -cp ./bin info.eoinkelly.HelloWorldApp

# create a jar file ( -C makes the root of the jar archive be ./bin)
$ jar cvfm EoinTest.jar manifest.txt -C ./bin .

# run the app
$ java EoinTest.jar
```
