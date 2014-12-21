# Memory and Javascript

An attempt to collect all my notes on this topic.


## Stack Vs. Heap

* http://stackoverflow.com/questions/79923/what-and-where-are-the-stack-and-heap

Stack

* Each thread gets a stack
* Stack is LIFO
* The size of the stack is set when the thread is created
* Used for function calls and parameters and return pointers


Heap

* In C stuff is put on the heap via new and malloc
* Can be very large
* Slower to allocate than the stack
* Used for objects

### Javascript Stack Size

* Varies by browser

Can test with

```js
var i=0;
function inc() {
    i++;
    inc();
}
inc();
```

Some stats from: http://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit

```
(number of iterations of a recursive function before we exceed stack size)

Internet Explorer
    IE6: 1130
    IE7: 2553
    IE8: 1475
    IE9: 20678
    IE10: 20677
Mozilla Firefox
    3.6: 3000
    4.0: 9015
    5.0: 9015
    6.0: 9015
    7.0: 65533
    8b3: 63485
Google Chrome
    14 (stable): 26177
    15 (beta): 26168
    16 (canary): 26166
Safari
    4: 52426
    5: 65534
Opera
    10.10: 9999
    10.62: 32631
    11: 32631
```

I got 25018 for chrome 23 on my laptop

# Creating Memory Leaks in JS

* The DOM is the parsed representation of the HTML
* The DOM is declarative
* They tried to design the DOM to work with any language from C to JS to others
* JS is OO by convention only!

JS Objects
* Separate memory heap/Advanced GC
* Cycles are not a problem

DOM Objects
* C/C++ memory heap/ RefCounting: malloc() and free()
* Cycles are a problem for browser develoeprs

* We can create a memory leak by connecting JS objects and DOM objects

```js
// jsObject (a JS object) has a reference to document.body (a DOM object)
var jsObject = { element: document.body }j:j:;

// document.body (a DOM object) now has a reference to jsObject (the JS object
// which has a reference to a DOM object)
document.body.myAttribute = jsObject;
```

* You have now created a circular reference because you have stuff on the 2 different heaps refering to each other
* Only a problem for long lived AJAX apps which don't reload pages

# Static memory JS talk

* https://www.youtube.com/watch?v=Op52liUjvSk

If you app uses X MB memory then you need 6X memory available for GC to be comparable to manual memory management.

Memory pools can help with GC pressure.


# Memory analysis with Chrome

https://developer.chrome.com/devtools/docs/memory-analysis-101

* Shallow size
    * the memory an object holds itself (the sum of the sizes of its immediate values)
    * Usually only String and Array have significant shallow sizes
    * Strings are often stored in renderer memory and only a small wrapper is exposed on the JS heap
* Immediate values
    * the values held directly in an object
* Retained size
    * shallow size + the memory held by other objects it references


* The heap
    * a graph (a network) of objects in memory
    * node = an object (labeled with the Constructor of it)
    * edte = a property
    * path = a sequence of edges
    * GC roots
        * ???
    * retaining path = any path from GC roots to a particular object
        * without a retaining path the object is eligible for collection
    * Dominator = a role that a node can play
        * A node is is a dominator if it exists in every simple path from the root to an object
        * If a dominator is removed all the objects it dominates can also be removed
        * Every object in the heap has exactly one dominator
        * This is making statements about the kind of tree the heap is
        * A dominator does not need to have a _direct_ reference to the objects it dominates
    * Accumulation point
        * An object (usually a collection) that retains a lot of memory
        * aka is a dominator for many objects

### Numbers

Numbers can be stored in two places

1. Immediate 31 bit integer values - so called "Small Integers" or SMIs
2. On the heap
    * if number cannot fit into the SMI form e.g. a double
    * if the number needs to be boxed e.g. to set properties. Presumably also if you call methods on it.

### Strings

Strings can be stored in two places:

1. Renderer memory
2. On the heap

* A string downloaded from the network (e.g. contents of a `<script>` tag) is not copied to the JS heap. It stays in the browser memory and a small wrapper object is created on the JS heap.
* V8 is lazy about concatenating strings. When you concatenate two strings their memory is not touched until absolutely necessary. Instead a "cons string" object is created that represents the concatenation. The concatenation really happens when you force it e.g. if you get a substring of the concatenated string.

### Objects

JS Object types:

1. Boolean
2. Number (double precision IEEE 754 float)
3. String (UTF-16 encode string)
4. Object (key value map)
    * key is *always* a string (will be coerced into string if you give it something else)
    * value is any type of the 4 values

Objects have a "retaining tree"  - the paths (1+) (node and edges) that prevent the object from being freed.

Objects have 2 sizes

1. Shallow size
    * Just the object
    * Usually a small constant vlaue
    * size = self
2. Retained size
    * Sum of the shallow size of the object and all descendants
    * size = self + desendants

NB: Shallow === Retained for Boolean, Number, String

Garbage is defined as "all variables which cannot be referenced from the root node.

Garbage colleciton is 2 phase

1. scan and find all garbarge
2. remove garbage and remove to system

Memory is split into "young" objects and "old" objects.

* When you call "new" you are pulling memory from the young memory pool.
* This is a cheap operation until ...
* If there is no available space in the young memory pool GC runs on it (and it alone)  - this can take milliseconds

Things to be aware of

* How often am I allocating new objects?
* How long do I hold on to them?

e.g. in a game you pretty much need to be able to do a game with zero allocations


* Objects (dictionaries) are backed by arrays in V8

* Arrays are the building blocks of JS objects

A typical JS object is made up of 2 arrays:

1. An array for named properties
2. An array for numeric elements

If the number of named properties is small then the array 1 will not be created - the properties will be stored internally in the object.

A "map" describes object kind and its layout

An "native object group" is a collection of objects that hold mutual references to each other.

Native objects are not represented in the heap - the have 0 size.

Instead wrapper objects are created.
    * each wrapper holds a reference to the corresponding native object and will send commands to/from it

## Javascript memory profiling

* https://developer.chrome.com/devtools/docs/javascript-memory-profiling


A page with no JS
    * Heap size is ~ 2.2 MB when page is first loaded rising to ~ 2.9 MB in a few seconds

Ember-cli ember app (no content, just basic app created, application route loaded)
    * Production build heap size: ~ 12.2 MB
    * Development:  ~ 15 MB
    * Development + Ember inspector: ~ 15.5 MB

# Chrome dev tools

* https://groups.google.com/forum/?fromgroups#!forum/google-chrome-developer-tools

# Sources of memory leaks in the browser:

What causes memory leaks in the browser???

# Chrome dev tools heap snapshot view

Source:
* http://addyosmani.com/blog/taming-the-unicorn-easing-javascript-memory-profiling-in-devtools/

Uses:
* Find objects that are unreachable by JS but are not collected (and should be)

V8 does a GC just before it takes the snapshot
NB: The heap snapshot does *not* show scalar values - it only shows objects
    Q: does it count scalars toward the sizes shown?

* (things)
    * Can ignore things in parenteses (I cannot control them)
* dimmed things
    Can ignore dimmed things (I cannot control them)



* @12345
    * an object unique ID
    * will be the same across heap snapshots so you can compare a particular object over time

* Red nodes
    * Red nodes do not have direct references from JavaScript to them, but are
      alive because they're part of a detached DOM tree.
    * There may be a node in the tree referenced from JavaScript (maybe as a
      closure or variable) but is coincidentally preventing the entire DOM tree
      from being garbage collected.

* Yellow nodes
    * Yellow nodes however do have direct references from JavaScript.
    * Look for yellow nodes in the same detached DOM tree to locate references
      from your JavaScript. There should be a chain of properties leading from
      the DOM window to the element (e.g window.foo.bar[2].baz).


> It's generally a good idea to begin investigation from the first object
> retained in your tree as retainers are sorted by distance (well, distance to
> the window). The object retained with the shortest distance is usually your
> first candidate for causing a memory leak.
