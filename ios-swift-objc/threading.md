# Grand central dispatch

* Based on _thread pool pattern_
* OS manages the thread pool - the dev injects _work packages_ into the pool without knowing its architecture
* GCD is the marketing name for the open source `libdispatch` library
    * there is some support on linux and windows for this!
    * apple ported it to windows for Safari & iTunes but that is not OSS

* A _task_ is a function or a block
    * are lightweight compared to traditional threads: 15 instructions to queue a work unit vs several hundred to create a thread
    * are much more efficient than creating a thread to do a single task
    * _tasks_ are used to create _work items_ (they are not the same thing)
* Tasks that _can_ be run in parallel are _queued_ for execution. The OS then schedules them
* GCD provides an abstraction on top of threads
* is a C API
* takes care of whether to create a new thread or re-use an old one when you give it a task to run

As well as creating a task and manually scheduling it you can create a task and assign it to an _event source_. Then when that event source _triggers_ an event the OS will create a _work item_ from the task and schedule it.


* Dispatch Source
    * an object
    * allows _the client_ to register blocks/functions to execute asynchonously
      upon system events e.g. a socket or file descriptor being ready for
      reading or writing or a POSIX signal
* Dispatch Group
    * an object
    * allows several _tasks_ to be grouped for later joining
    * Tasks can be added to a queue as a member of a _group_ and then the client can use the group object to waitin until all the tasks in that group have completed.
* Dispatch Semaphore
    * an object
    * allows a _client_ to permit only a certain number of tasks to execute concurrently
* Dispatch Queue
    * an object
    * manages a queue of _tasks_
    * has a priority level
* Serial Queue
    * an object
    * manages a queue of _tasks_
    * Guarantees that the tasks will be run in the order added to the queue
    * A good alternative to using a _lock_ on a resource.
    * You can add tasks to a queue from a queued task

The library automatically creates several dispatch queues each with different
priority levels. It select the number of concurrent tasks to run based on
inspection of its operating environment.

Terminology

* Task = function or block
* Client = any code that uses `libadispatch`


There is a "main queue" of the applicaiton that runs on the main thread of the app.

```objc

// Run a block on a different queue
dispatch_async(<queue name>, <block>);

// Get a reference to the "global queue"
dispatch_get_global_queue(<queue priority, 0)

// Get a reference to the "main queue" (the application thread runs on this)
dispatch_get_main_queue()

// call the given block a given number of times. This is blocking so you can
// grab the result on the next line
dispatch_apply(<num times to call block>, <queue name>, <block>)

// create new serial queue
dispatch_queue_t exampleQueue;
exampleQueue = dispatch_queue_create("com.example.some.unique.identifier", NULL);

// use exampleQueue

// release the queue
dispatch_release(exampleQueue);
```

## NSOperations

* built on top of GCD
* features
    * you make operations dependent on other operations
    * can reorder operations after you have submitted them

    TODO: Semaphore

    TODO: learn about POSIX signals

