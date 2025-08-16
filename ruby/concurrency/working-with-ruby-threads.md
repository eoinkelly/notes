# Working with ruby threads

MRI creates two threads by default.

1. One is the main thread
1. and one does housekeeping
    - called `ruby-timer-thr`

It seems like Thread and Queue are automatically defined for me and
`require "thread"` is not required ???

### Creating a new thread

```
thread = Thread.new { }
thread = Thread.fork { }
thread = Thread.start(1,2) { |a, b| ... }

TODO: differeences between these ways of creating a thread ???
```

After a thread is created you can call `#join` on it to cause your current
thread to wait for it to finish i.e. to "join that threads execution to the
current thread of execution"

Threads finish with either:

1. They come to the end of their block
2. They raise an unhandled exception
    - it ends the thread
    - the exception will be re-raised when another thread tries to join with it

### Thread#value

- Calls Thread#join and returns the value of the last expression in the code
  block
- It will re-raise any exceptions which occured in the joined thread

### Thread#status

- shows status of the thread
- values:
    1. `run` - this thread is currently running
    1. `sleep` - the thread is currently
        1. sleeping
        2. blocked waiting on a mutex
        3. blocked waiting on IO
    1. `false` - the thread finished its block of code or was successfully
       killed
    1. `nil` - this thread has raised an unhandled exception - be wary when you
       join with it
    1. `aborting` - the thread is currently running but dying

Examples

```
Thread.current.status # => "run"
```

### Thread.stop

- operates on the **current** thread
- Notice that this is a class method on Thread but `wakeup` is an instance
  method
- does the following
    1. put the current thread to sleep, tells the thread schedulre to schedule
       another thread
    1. keep the current thread from being scheduled again until `Thread.wakeup`
       is called

### Thread.pass

- operates on the **current** thread
- Notice that this is a class method
- **hints** to the thread scheduler to schedule something else this time but
  keeps the current thread available for scheduling
- it is kind of like passing on a turn in a game
- there is no garuantee that the thread scheduler will take the hint

### Thread.wakeup

### Thread#raise

- DO NOT USE
    - backtrace will point to whatever line was executing in the thread when you
      forced the exception which is pretty useless for debugging
    - it will skip any `ensure` blocks defined inside the thread so might miss
      out on important cleanup
- allows you to raise an exception inside a thread from another thread

### Thread#kill

- DO NOT USE
    - backtrace will point to whatever line was executing in the thread when you
      forced the exception which is pretty useless for debugging
    - it will skip any `ensure` blocks defined inside the thread so might miss
      out on important cleanup
- allows you to kill a thread from another thread
