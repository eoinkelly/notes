
"basic addition"
2 + 2.

"send messages with space"
". terminates an expression ???"
'XXVI' romanNumber.

"create an array literal that will be evaluated at compile time"
compileTimeArray := #('foo' 'bar' 'blah').

"arrays are 1 based!!!"
"message send with space"
"message args with arg names like objc"
compileTimeArray at: 1.

"syntax for array literals that will be evaluated at runtime"
"notice the computation in the array"
runTimeArray := {23. 44. 5+6}.
runTimeArray at: 1.


"block syntax"
[ |a b c| a + b + c ]
"blocks are first class object"


"indicate a return value"
^foo
"can be read as: return foo"

"smalltlak has no implicit receivers - every message has an explicit receiver"
"has 3 kinds of messages"

"1. unary message"
16 sqrt.

"2. binary message"
2 + 5.
"very similar to how ruby does it i.e. 2.send(:+, 5)"

"3. keyword arguments"
runTimeArray at: 3 put: 4.
"that is the 'at put' method - method names come from list of keywords"

"smalltalk has not operator precedence"
"everythign evaluated left to right, unary messages first, binary messages second, keyword messages third"
2 + 3 * 5.
"=> 25"

"can use parens to control order of operations"
2 + (3 * 5).
"=> 17"


"conditionals, classes, loops are not in the lanuage but the library"

"if-else"
(22 > 3) ifTrue: [ 'YES' ] ifFalse: [ 'NO' ].
"(22 > 3) returns true"
"true is then passed the ifTrue:ifFalse: message"
true ifTrue: [ 'YES' ] ifFalse: [ 'NO' ].
false ifTrue: [ 'YES' ] ifFalse: [ 'NO' ].

"true is an instance of TrueClass"
"false is an instance of FalseClass"
"each of those classes has an implementation of ifTrue:ifFalse: method"


"system browser contains ALL the code for your smalltalk environment"

" The system browser looks like: "
" | package | class | protocol | methods | "
" |       details                        | "

pacakges are groups of classes to help humans categorise
protocols = groups of operations
