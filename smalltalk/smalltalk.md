```smalltalk
"basic addition"
2 + 2.

"send messages with space"
'XXVI' romanNumber.

"create an array literal that will be evaluated at compile time"
compileTimeArray := #('foo' 'bar' 'blah').

"arrays are 1 based!!!"
x at: 1.
```

localVariable

- starts w. lowercase
- inner caps by convention

GlobalVariable

- starts with uppercase
- globals and constants

Keywords

self super nil true false

"character literals" $a

'strings are single quotes' "comments are double quotes"

#symbol #('an' 'array' 'of' 'strings')

- note no commas between elements
