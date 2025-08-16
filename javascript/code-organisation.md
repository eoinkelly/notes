# Code organisation

## Sources

I surveyed these repos to see how modules are organised in fairly large
projects.

- [vscode](https://github.com/microsoft/vscode)
- [prisma](https://github.com/prisma/prisma)
- [redwood](https://github.com/redwoodjs/redwood)

## Q: How should I organise related modules?

I'm unsure how common this approach is

```bash
# foo has modules which only it uses
src/foo.ts
src/foo/helper.ts
src/foo/other.ts

# bar has no related modules
src/bar.ts
```

## Q: What naming scheme to use for files?

A: Default to `camelCase.ts`. Use `PascalCase.ts` if file default exports a
class or is a react component

- camelCase seems very common - prism, vscode, redwood all use camelCase file
  names
- the file is named after either its single export or something that groups the
  exports together
- If there is a default export and it is a class then the file is
  `PascalCase.ts`

## Q: What naming scheme to use for directories?

A: Use kebab-case

- projects seem to try to have just one word directory names whenever possible
- kebab-case seems the most common approach
    - kebab-case
        - npm packages
        - prism
        - redwoodjs
    - camelCase
        - vscode (some of the time, also extensively uses packages)

## Q: Should exported names go at end of file or start?

A: I put exported symbols at end of file but unsure if that is most idiomatic

```
pros/cons of putting at end:

* -- you have to scroll down to get to the code you probably want to read first
* ++ a consistent approach helps
* ++ doesn't matter whether functions defined with `=>` or `function` because the dependent things are defined first

What I've seen in the wild:

* prisma - interleaves export and non-export
* VS Code - interleaves

There doesn't seem to be any consistency here
```

## Q: When to organise with a module vs a class?

- Default to using modules.
- Use classes only if you need instances with their own state.
    - you might need a class if
        - Are there multiple of this thing in the system e.g. users

## When to create a new package

Larger projects make heavy use of separate packages (with their own
`package.json`, typescript and test config files) e.g. redwood uses top-level
`./packages` dir to hold them.

This is presumably something you refactor into as your project grows (unless
there are clear architectural lines from the start).

# How JS differs from Ruby & Python

Modules can be organised in directories as you see fit but that organisation
doesn't influence the symbols you import.

This changes how you think about how modules depend on each other.

The path seems to be

1. put extra functions in the file
2. create a new file
3. create a dir for files
4. create a package

The exported symbol and the file+folder path are decoupled in JS each module
decides what symbols it pulls in and can rename them at import symbols don't
seem to ever be "namespaced"

In ruby we can only import a whole file - we cannot pick and choose symbols
within it and have no easy way to for rename the names we import.

This leads to a convention of one symbol per file (the class) and then all the
symbols you actually use are namespaced under it.

Namespacing your imported symbols is easy to do in JS/TS but it doesn't seem to
be a common practice.

JS makes the namespacing optional and mostly people don't seem to do it:
pros/cons of JS approach _ ++ editors make it easy to see where a symbol came
from if you need to know _ ++ less coupling of the symbols - you could change
one exported symbol without impacting others in a module _ -- I can't easily see
where everything came from when I look at JS code - the coupling between modules
is less obvious _ ?? in some ways it is more obvious tho because it's explicit
at top of file? _ it's less obvious in symbol usage _ I could mimic the ruby
style in JS easily enough but it doesn't seem idiomatic \* I am used to seeing
the coupling everywhere in the file not just the top

in python it is more normal to pick and choose names ot import from a module but
it has much stronger rules than JS about how the module name is mapped to a file

JS `import` can act as Ruby `require_relative` if you pass it a relative path or
Ruby `require` if you pass it a package name.

in JS the import statement imports diretly from a filepath, not a module name
that gets resolved to a file path except if you are importing from an npm
package python imports from a symbol ruby imports from a file-path
(require_relative) or a name (which is resolved to a file on disk)
(`require 'foo'` becomes `<gems_path>/foo/lib/foo.rb`) python has "pacakges", JS
does not java has the notion of a `package` which is an abstraction over
files/folders on disk python has rules for how module names (separated by `.`)
are turned into files/folders on disk ruby itself does not have rules like
python but rails autoloader (now zeitwerk) adds them zeitwerk uses constant
namespacing to figure out what file to load JS does not have a zeitwerk

vs code doesn' tuse the relative path style of module names e.g. `./`. it starts
everything from the root of the project e.g.

```ts
import * as arrays from 'vs/base/common/arrays';
```

The path in `... from 'path';` is interpreted by the JS environment. How it
interprets it can vay a lot. Node has one set of rules, browsers with importmaps
have another. Node
[may get importmap support](https://github.com/nodejs/loaders?tab=readme-ov-file#milestone-3-usability-improvements).

importmap is an abstraction over the location of files (either on disk or over
http or whatever) but you'll still want your files on local filesystem to use
paths so you can keep it straight in your head a bit easier.
