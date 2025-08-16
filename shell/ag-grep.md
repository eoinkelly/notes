# ag (silver searcher) - faster replacement for grep

PATTERN = PCRE regular expression (same as ack)

-F = fixed string (interpret pattern as a plain string not a regex)

```sh
ag "some-regexp"

# limit search to particular file types
# use --file-types to see a list of them
--FILETYPE
ag --haml PATTERN
ag --sass
ag --ruby
ag --js

# ignore .agignore, .gitignore etc.
ag -a "regexp"

# use smart case
-S

# ignore files & dirs that match this pattern
--ignore PATTERN

# show NUM context lines around matches
-C NUM
```
