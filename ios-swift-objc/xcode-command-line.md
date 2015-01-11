

`xcodebuild`

* command line access to XCode IDE

* a tool for batching work on a project or workspace - not really designed for fine-grained

```
$ cd /path/to/code/Xcode/projectdir
$ xcodebuild # equivalent of Cmd-R in GUI
```


Xcode has its own version of the command line compiler and linker tools in its bundle e.g. `/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang`

I presume the "xcode command line tools" install is what gives me `/usr/bin/clang` ?

You can use `xcrun` to find or run versions of the tools installed inside Xcode e.g.


```
$ xcrun --find nm
$ xcrun nm # runs it
```

TODO: what does the `nm` tool do?

```
# Xcode comes with quite a few built-in tools
$ ls /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
ar                cc                ctf_insert        gm4               lipo              nm                rpcgen            swift-stdlib-tool
as                clang             dsymutil          gperf             llvm-cov          nmedit            segedit           swiftc
asa               clang++           dwarfdump         indent            llvm-profdata     otool             size              unifdef
bison             cmpdylib          dyldinfo          install_name_tool lorder            pagestuff         strings           unifdefall
c++               codesign_allocate flex              ld                m4                ranlib            strip             unwinddump
c89               cpp               flex++            lex               mig               rebase            swift             what
c99               ctags             gcov              libtool           mkdep             redo_prebinding   swift-demangle    yacc
```


To dump all the build settings for a particular target use:

```
xcodebuild -showBuildSettings -project ./Katas/Katas.xcodeproj -target Katas
```
