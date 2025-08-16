Notice that

- the file names of the .go files don't matter when building a package
    - they do matter when building a executable if you pass a list of file names
      to `go build` - it will use the first filename as the name of the
      executable
    - if you pass `go build` a dir then it uses the dir name as the executable
      name
- go doesnt want snake_case or camelCase in package names - it wants all
  lowercase e.g. `examplething`

You cannot compile a go package w When you go build ./examplething go build
./examplething -o . it does the compilation but throws te output away - it just
checks that it can be compiled
