# look(ahead|behind)

```
(?<!a)b   # matches a "b" that is not preceded by an "a (negative look behind)
(?<=a)b   # matches a "b" only if it is preceded by an "a" (positive look behind)
a(?!b)    # matches "a" only if it is not followed by a "b" (negative lookahead)
a(?=b)i   # matches "a" only if it is followed by a "b" (positive lookahead)
```

Oddities

* JS regexps do not support negative lookahead/lookbehind

