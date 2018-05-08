# Writing thread safe code in Ruby


## The GIL

> MRI 1.9 prevents the Ruby code itself from running in parallel by requiring
> that any thread running Ruby code hold a lock. This lock is commonly knows as
> the GIL (Global Interpreter Lock) or GVL (Global VM Lock).

> the GIL is released if a Thread is waiting on an external event (normally IO)
> which improves responsiveness. MRI 1.9 also includes an experimental API that
> C extensions can use to run some C code without the GIL locked to utilize
> parallelism. This API is very restrictive though because no Ruby object may
> be accessed in any way while the GIL is not held by the current thread.

## Gems

* https://github.com/grosser/parallel
* https://github.com/ruby-concurrency/concurrent-ruby
* https://github.com/celluloid/celluloid
* https://github.com/eventmachine/eventmachine

## Ruby implementations

* MRI
    * Threads in MRI ruby can improve delays caused by IO waits.
    * They cannot help you utilise more cores
    * uses native threads to implement `Thread`
* JRuby
    * Thread can be used to solve problems where you want to use more than one core
    * layers the Thread class on top of Java’s thread class so it uses whatever the JVM does
        * The JVM maps java threads to an OS thread

## Ruby tools

The following classes are helpful for writing thread safe code in Ruby

* `Thread`
* `Queue`
* `Mutex`

## Multi-threading in Rails

http://edgeguides.rubyonrails.org/threading_and_code_execution.html

QUESTION: does spawning threads from a rails app a problem good/bad depending on which rack server you use e.g. unicorn vs puma?

