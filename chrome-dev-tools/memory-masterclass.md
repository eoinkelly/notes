# Memory analysis 101

* https://developer.chrome.com/devtools/docs/memory-analysis-101

## The heap graph

* A heap **is** a graph of interconnected objects
* Nodes and edges both have labels
	* Node label = name of constructor function that made the object
	* Edge label = name of property that provides the connection

A sequence of edges is a "path". There are many paths in the heap but we
usually only care about "simple paths" i.e. those that do not go through the
same node twice.

A "retaining path" is a path from a GC root to an object.

### Dominators

* A "dominator" of an object A is an object that sits in **every** simple path from the root to A. It means that if the "dominator" is removed then A will be unreachable so can be collected.
* Each object in the graph has exactly **one** dominator.
* A dominator does not necessairly have a direct reference to the object it dominates
* Consequences: dominators form a tree structure
* A node in the tree that dominates many objects may retain large amounts of memory and is called a "accumulation point".


TODO: finish this and then read https://developer.chrome.com/devtools/docs/javascript-memory-profiling#memory_mode


====================================

# Memory Heap Snapshot Profile

* https://developer.chrome.com/devtools/docs/javascript-memory-profiling#memory_mode

## Terminology

* Constructor
* Distance
* Objects Count
* Shallow Size
	* The memory held by an object itself
	* All JS objects have some memory set aside for description and immediate values.
*	* arrays and strings can have big shallow sizes
		* exception: strings are sometimes stored in renderer memory and only a small wrapper object exposed in the JS heap.
* Retained Size
	* The amount of memory that would be freed when the object is deleted
	* includes size of any objects that it references
	Q: does it only count referenced object for which this object is the last referrer or does it count every objec this object refers to ???


## What does healthy memory allocation look like?

* A profile for a healthy app is fairly long saw tooth curve
* These indicate allocations and then the sharp drop as GC kicks in.

## Unhealthy memory allocations have the following signs:

* A sharp, fast sawtooth is bad because it indicates you are allocating memory very quickly
* The slope of the upslope of the saw tooth indicates the rate of allocations
* If you have leaks then the sawtooths will trend upwards over time

## Counters

When you record a timeline and capture memory you get a memory panel showing graphs for:

* Used JS Heap [MIN:MAX]
	* measured in bytes
* Documents [MIN:MAX]
* DOM Nodes [MIN:MAX]
* Event listeners [MIN:MAX]

* The MIN:MAX are the minimum and maximum values for the selected time range
* These numbers represent the no. $things that are still held in memory
i.e. they have not been GC'd yet. Not all of these have to be attached to the
DOM.
* The graphs have time on the X axis but the Y axes are totally different and not shown


================================================

# Memory management basics

* http://www.html5rocks.com/en/tutorials/memory/effectivemanagement/

JS memory is conceptualized as a graph

JS has 3 primitive types

1. Number
2. boolean
3. String

These are always leaf nodes on the graph

JS has primitive 1 container type: the object
	* A non empty object is represented in the graph as an inner node with edges going to other nodes

Arrays in JS are actually objects with numeric keys. JS runtimes will optimize "array like" objects and represent them as real arrays under the hood.

## Terminology

* Value = an instance of a primitive type e.g. Object, Array, Number
* Variable = a name that references a value
* Property = a name in an object that references a value

All _values_ in JS are in the object graph. 
The graph begins with 1 or more _roots_.

## GC Roots
* roots in the object graph
* example: window object
* devs cannot control them, browser creates and destroys them
* note: global variables are not roots, they are properties on the `window` object.

A _value_ becomes garbage when there is no path from a (any) root to it
or
	QUESTION: how do closures work in here?

An example of a memory leak: You create a DOM node and a reference to it in `foo`. You attach it to the DOM and later it gets detached from the DOM by some other call. As long as the `foo` reference exists, the value is not garbage.

Defn. Bloat: 

* Your page is using more memory that the device can comfortably provide

Defn. Gabage Collection: 

* The browser decides when to do it
* It has to stop all JS execution to keep things sane
* Starting at the roots and exhaustively searching all the object properties
  and variables taht are alive in the stack frame, a value cannot be reached,
  then it has become garbage.

## How V8 does GC

Memory is divided into 2 generations

1. Young - allocation and collection happens quickly here
2. Old - allocation and collection happens less frequently

* Each generation has its own heap
* "Age" of a _value_ = the no. of bytes allocated since the value was allocated
  but often approximated by the no. of young GCs it has survived.
* When a value is "old" enough it is "tenured" into the old generation.
* Studies show that approx 70% - 90% of values do not make it to old generation

Young GC 

* It has its own heap, split into two spaces - the "from" and "to" spaces.
* All allocation happens in the "to" space until "to" is full at which point
  the young GC runs.
* A young GC run will
	* swap the "from" and "to" spaces
	* go through each value in the new "from" space and eithe
		1. copy it to the new "to"
		2. tenure it out to the old generation heap
* A young GC run typically takes ~ 10 ms. 
	* An implication of this is that if you want 60fps (26ms frame time)
	  you cannot do **any** allocations because a single young GC run will
	  pause your app for most of 1 frame.

Old GC

The old GC heap uses a "mark compact" algorithm 
An old GC run will happen whenever a value is tenured from the young generation to the old generation. Implications:
	* An old GC run always follows a young GC run so its time required will just be added to the young GC run.


The performance.memory API shows 3 things

1. jsHeapSizeLimit - the amount of memory (in bytes) that the JS heap is limited to.
	TODO: what does this mean. is this young + old heaps?
2. totalJsHeapSize - total amount of memory including fee space that JS has allocated
3. usedJSHeapSize - amount of memory currently being used


> Contrary to the popular belief that “more memory results in better
> performance”, the Gmail team found that the larger the memory footprint, the
> longer latencies were for common Gmail actions. 

TODO: Why does a bigger memory usage mean more latency in response if there is enough memory???

TODO: learn the 3 snapshot technique for finding memory leaks
