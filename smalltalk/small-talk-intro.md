# Problem solving
An initial problem solving approach in Smalltalk is to try to reuse the existing objects and messages.

# The smalltalk environment
* The editor, compiler, linker, debugger and the application in Smalltalk are part of the runtime image. 
* This allowed users to run the smalltalk program while changing the source code. Changes made to the source code are reflected in the running application instantly.

* Is garbage collected
* The code that is associated with each message is called a method.
* Smalltalk distinguishes variable identifier and a method identifier by the identifier's position in the expression.

The object-oriented problem solving approach, 
1. Identify the problem
2. Identify messages to be sent
    * express the problem as a set of messages - can this be done without thinking of at least some of the objects first?
3. Identify the objects needed for the solution
    * can an objects functionality just be the sum of the messages it sends/receives
    * once i can visualise that functionality, then I can name the object
4. Create a sequence of messages to the objects that solve the problem.

* Polymorphism allows two or more objects respond to the same message.

## keyboard shortcuts
* cmd+shift+click opens a find-window menu
* cmd-p = print it
* cmd-d = do it

* click = normla click
* ctrl+click = action click
* shift+option+click = meta click

```smalltalk
1 + 2     "send message '+' to 1 with argument 2"
Date today asTime "send message today to Date, then send asTime to the result of that
```

* literal arrays are created by
```smalltalk
#(1, 3, 5) "create a literal array"
```

* single quotes create a string
```smalltalk
'this is a string in smalltalk'
```

* space is the 'send a message' operator 

# Naming conventions

* Class names begin with an uppercase letter as in ruby
* message names begin with a lowercase letter and are camelCase


# 3 Types of messages

* message name != method name

## unary message
* takes no parameters
```
5 factorial
Date tomorrow
"Object messageName"
```

## binary message
* used for 3 types of operators
    * arithmetical
    * comparison 
    * logical
* can be 1-2 chars long
* must be chars from set: + / \ * ~ < > = @ % | & ? ! ,
* examples:
```
a + b   "send the message + to a with paramater b"
a <= c
a | b
```

## keyword message
```
AnObject aName1: parameter1 aName2:parameter2   "aName1 and aName2 are part of the message name"
                                                "parameter1, parameter2 are the actual parameters passed"
Array new: 20
TheDate month: currentMonth year: currentYear   "equivalent in other lans to TheDate.month(currentMonth:month currentYear:year)"
                                                "or TheDate.month(month, year)
"the message name is month:year: (notice the :'s)"

```

* executable statements end in `.` except 
* if they are the last statement in a method
* interfact definitions do not end in `.`

* smalltalk is evaluated from left to right - these lines are equivalent:
```smalltalk
3 + 4 + 5.
(3+4) + 5.
```

* comments are multi-line and are begun and end with "
* strings are concatenated with ,

```smalltalk
'hello' , 'foo'  
```

* variables are pointers to objects (like in ruby)
* objects have a uniqe object ID ??? how to get it?
* create a new instance of an object by sending the new message to the class
* new returns a pointer to the created object

* assigment is done by `:=`
```
foo := Foo new
```
* waht does `=` do?

* within a class, `self` points to the class. You can use it as a receiver to send messages to yourself


* return from a method using ^

## Order of message execution
The rules that Smalltalk use when deciding the order of exercuting messages can be summarize as the following:

1. Smalltalk executes messages from left to right.
2. The result of a message replaces that message in the statement.
3. Smalltalk executes all expression that appear inside a pair of parenthesis first, with the left-most inner pair of nested parenthesis.
4. Inside an expression, unary messages are executed first, followed by binary messages, followed by keyword messsage, in a left-to-right direction.
    * what are consequnces of this? edge cases?
5. Smalltalk executes all binary messages from left to right, regardless of what operations they perform. **This means there is no special order of execution for arithmetic operations.**
6. An expression can include a variable name. Smalltalk replaces the variable name with the object to which it points.

Try not to care. Beginning Smalltalk programmers often have trouble because they think they need to understand all the details of how a thing works before they can use it. This means it takes quite a while before they can master Transcript show: 'Hello World'. One of the great leaps in OO is to be able to answer the question “How does this work?” with “I don’t care”.

If you talk to Smalltalkers for a while, you will quickly notice that they gen- erally do not use expressions like “call an operation” or “invoke a method”, but instead they will say “send a message”. This reflects the idea that ob- jects are responsible for their own actions. You never tell an object what to do — instead you politely ask it to do something by sending it a message. The object, not you, selects the appropriate method for responding to your message.

* Become comfortable with throwing away images regularly - they are not for managing code e.g. monticello

* The transcript is the smalltalk system log - write to it as
```
Transcript show: 'some message'; cr.
```

## print it
* 'print it' actually 
  1. compiles the expression, 
  2. executes it, 
  3. sends the message printString to the result, 
  4. and displays the resulting string.

## inspect it
* opens an inspector window that lets you
    * see instance variables in the class
    * send messages to the class

## explore it
* The explore view is similar to the inspector but it shows a tree view of the object and its parts - you can explore it in memory - this is really cool!
    
# have read until
up to end http://www.inf.ufsc.br/poo/smalltalk/ibm/tutorial/chap2.html
up to p15 in pharo book


"
To browse classes

1. send the browse message to the class
Classname browse

2. highlight class name anywhere and cmd-b

To search for classes, do cmd-f after clicking in the **package browsing** pane of the browser


Pharo also includes support for true block closures

Alan Knight:
Try not to care. Beginning Smalltalk programmers often have trouble because they think they need to understand all the details of how a thing works before they can use it. This means it takes quite a while before they can master Transcript show: 'Hello World'. One of the great leaps in OO is to be able to answer the question “How does this work?” with “I don’t care”.


invoking pharo
shift+option+click == meta-click = open the morphic halo
ctrl-click = cmd-click ==  action-click = show the context menu
click = 

PharoMorphic/DemoAndExamplePackage

smalltalk has rational numbers

(1/3) + (2/3) => 1 the brackets are required here

## metaclasses
an anonymous class with only one instance e.g. Point
Point class #=> Point class

so 'Point class' is the just name of a class in smalltalk. in ruby they just begin with a capital letter, in smalltalk they begin with capital but end in ' class'

You never tell an object what to do, instead you **politely ask** it to do something by sending it a message - the object, not you, selects the approprate method for responding to your message

Smalltalk uses the #initialize metthod similarly to how ruby does

## Point short-hand

```
12@13
```
sends the message '@' to 12, with the argument 13. The '@' message causes the Number to as Point to create a new point with the given co-ordinates.

## Packages

Packages and categories are almost the same thing. By convention they have the same names. Packages are a superset of categories.

* category = a collection of relate classes
* package = a collection of related classes **and** extension methods

# Sources

* http://www.inf.ufsc.br/poo/smalltalk/ibm/tutorial/content.html

