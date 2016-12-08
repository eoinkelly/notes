# Tenderlove talk on ruby memory management

Source: https://www.youtube.com/watch?v=r0UjXixkBV8

Objects in ruby object space form a tree structure

GC is responsible for two things

1. collection side algorithm
1. allocation side algorithm


Terminology

* "Root set" = special varialbe which are referenced from the top level
* garbage = nodes which are no longer referenced from a root node
* live data = nodes which are actually referenced from the root node

QUESTION: is there only one root node in ruby?

## Collection algorithm

MRI GC collector is a

* mark and sweep GC
* generational GC

###  mark and sweep GC
* two phases
    1. mark phase
        * follow all the relationships between nodes and mark them if they should be retained
        * NOTE: marked nodes are retained, unmarked are deleted
    2. sweep phase
        * delete all unmarked nodes
        * then unmark everything
* pros/cons
    * ++ easy to implement
    * -- slow - it has to "stop the world"
    * -- it has to visit *every* object *every* time

###  generational GC

* helps you walk fewer objects
* object usually die young
* divide objects into "old objects" and "new objects"
* as objects (nodes) survive GC passes they are moved into "older" generations
* with a generational collector you don't have to visit every node every time
* but you might have a case where new objects are referenced only by old
    objects so if GC only looks at new objects and not old it would look like
    the new object had no references and could be deleted.
* Ruby works around this by
    1. having a "write barrier" piece of code which will exec if an object
        is linked from old gen to new.
    1. the write barrier adds the new object to the "remembered set" which
        prevents GC from deleting it
* pros/cons
    * ++ faster
    * -- harder to implement
* they are still "stop the world"
    * creating a "remembered set"
* objects are considered young until they survive three GC cycles

### incremental GC

* uses three colors for marking (aka a "tri-color algorithm")
    * white
        * will be collected
    * black
        * no reference to white objects but is reachable from root
    * grey
        * reachable from root but not yet scanned
* algorithm
    1. pick object from the grey set and move it to the black set
    1. for each object it references move it to the grey set
    1. repeat the steps above until the grey set is empty
* pros/cons
    * we can interrupt any of the steps - we don't have to do the full tree every time
        * halting time is reduced
    * -- we still need the write barrier and remembered set because there can be states where a newly created object could be mistakenly collected.

## The ruby GC

* runs concurrently but not parallel to tyour program
* is not real-time - it will not garuantee to be done within a particular time
* is not compacting

Remember: a ruby page is not the same as an OS page (ruby page is 16Kb, OS page is 4Kb

Ruby's GC is also responsible for allocating memory. This is how it works:

* Ruby does not `malloc` every time you create an object (`malloc` costs in CPU time)
* Ruby allocates a large contigious chunk of memory (called a "page" or "slab")
    * default page size is usually 16 KB (dependso on CPU architecture a bit)
    * pages are "aligned" (page addresses are divisible by multiple of 40)
        * => that ruby object addresses must end in 000b
        * => you can tell by inspection of address whether that address holds a ruby object
* Inside that chunk of memory is a linked list (called the "free list")
* nodes in that linked list are called slots
    * the free list (usually) has 408 slots (if 16KB page size
* each slot is a ruby object
    * each slot is 40 bytes

new objects are allocated by checking the pointer to the end of the "free list"
and putting the object there then "bumping the pointer" to the next free slot.
This process is called "bump pointer allocation"

"Eden pages" are the pages which are searched for free slots

If every object in a page has been collected then the page is put in a "tomb"
where it can later be given back to OS

## aside: allocation hacks

* tagged pointer technique:
    * can represent small values without allocation
    * in integers we need 1 bit for sign and 1 bit for flag which indicates it is a tagged integer (not an ordinary ruby object) so the maximum size of int that will not allocate an object is  <= (2^62 - 1)
    * ruby objects which are tagged pointers
        * fixnum
        * float
        * true/false
        * some symbols

## Introspection of ruby memory management

TL;DR: read docs for `GC` and `ObjectSpace`

```ruby
GC.stat
GC::Profiler.enable
GC.start
GC::Profiler.report

require "objspace"
require "json"

ObjectSpace.dump_all # json representation of the entire ruby heap (doesn't show empty slots)

# dump info about an individual object
xx = "foo"
JSON.parse(ObjectSpace.dump(xx))
```
