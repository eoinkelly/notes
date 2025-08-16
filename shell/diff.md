# diff

- Put the old version first on the command line and then the + and - in the
  output will make sense
- Use -u as this is what git uses so I only have to keep one diff format in my
  head
- Use -b as I almost never care about whitespace changes

```
diff  <options> <old-version> <new-version>
-y = two col output
-u = unified output
-b = ignore whitespace changes
-B = ignore all whitespace
```

## Unified output format

http://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html#Detailed-Unified

```
--- <from-file> <from-file-modification-time>
--- <to-file> <to-file-modification-time>
```

- starts with two line header
- the 0 or more "change hunks"

within a change hunk

```
@@ <from-file-line-numbers> <to-file-line-numbers> @@
```

- line numbers are of format `start,count` where
    - start = the number of the first line in the chunk
    - count = the number of lines in the chunk
- the header is in format `@@ STUFF @@`
- unchanged line preceded by space
- added lines preceeded by +
- removed lines preceeded by +
- a changed line is show as one addtion and one deletion
