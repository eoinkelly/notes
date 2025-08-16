# J language

https://www.jsoftware.com/help/learning/contents.htm

```
# install J
$ brew cask install j

$ jcon # runs the interactive console
$ jconsole # alias for above
# Ctrl-d to exit

$ jhs # runs a local web server console
# TODO: how to close it? ctrl-c and -d don't seem to work

$ jqt # runs a QT based console
```

- array based
- kind of an ASCII friendly APL
- written in C but seems to have some Java GUI stuff
- filetype is `.ijs`
- comment marker is `NB.`
- adjacency is important
- calls data "nouns"
- calls functions "verbs"
- negative numbers have underscore prefix rather than dash e.g. `-2` is `_2` in
  J
- `%` is the division symbol because APL used `/` to represent fold
- strings are single quote delimited

```j

NB. create a noun which is array [1,2,3]
1 2 3

NB. negate the array [0, 2, 5]
- 0 2 5
NB. 0 _2 _5

NB. infix + verb
1 + 3
NB. 4

NB. infix + verb between array nouns
2 3 + 4 5
NB. 6 8

NB. division by 4/1 as rational number
1 2 3 4 % 4r1
NB. 1r4 1r2 3r4 1

NB. division by 4
1 2 3 4 % 4
NB. 0.25 0.5 0.75 1

NB. negation of a noun
- 0 2 5
NB. 0 _2 _5

echo 'hello'
NB. hello

NB. folding a noun with +
+/ 2 3 4
NB. 9
```
