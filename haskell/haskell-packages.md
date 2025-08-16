# Haskell Packages

## .o (Object files)

- contain executable code on Mac OS

## Library files (.a, .dylib)

- .a = static library
    - if you link against this it will be copied into your binary
- .dylib = dynamic library
    - if you link against this, you have to include it when you distributre you
      app
    * can be loaded at runtime +makes app binary smaller
    * if the .dylib changes you don't have to recompile

- both .dylib and .a libraties come with header files
    - xcode project needs to know about the headers too
- you can add a library of either kind to Xcode by dragging the file (.a or
  .dylib) into Xcode

## .hi files (Haskell Interface files)

- When GHC compiles a module it creates a .o object file and a .hi interface
  file
- the interface file is used by GHC when other modules include your module
- you can think of them as compiler readable version so the .of file
- GHC will make the interface file much more details if you pass it the `-O`
  flag
- There will be 1 `.hi` file for each `.hs` module file in your app

## Cabal

```
$ cabal update # update the list of packages
```

- cabal keeps everything in `~/.cabal/`
    - `~/.cabal/packages` = the haskell source file pacakages (tar.gz files)
    - `~/.cabal/lib` = the compiled modules
