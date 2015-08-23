# JS tasks and microtasks

* https://jakearchibald.com/2015/tasks-microtasks-queues-and-schedules/

All windows from the same "origin" share an event loop so they can
synchronously communicate

Each web worker "thread" gets its own event loop

* Each event loop has multiple "task sources" e.g.
    * indexdb spec defines a task source for itself
* Order is garuanteed within a "task source"
* The browser gets to decide which "task source" to run next
    * it gives preference to performance sensitive task sources e.g. user input

## Microtasks

* examples of microtasks
    * mutation observer callbacks
    * promise callbacks
        * when a promise settles it queues a microtask for its reactionary
          callbacks
* scheduled for things that should happen after the current script is finished
  but don't need to get a full "task"
* any queued microtasks should execute *before* the the next task does
* any additional microtasks added during processing of the microtasks queue are
  executed in the same microtask queue run
    * a bit like the ember runloop
* note that the browser does not get to update rendering between the original
  script and running the microtask queue
* microtasks are known as "jobs" in ECMAscript spec

> The microtask queue is processed after callbacks as long as no other
> JavaScript is mid-execution, and at the end of each task.


* A bubbling event that is handled at multiple places in the DOM is all part of
  the same task i.e. new tasks will not be run until the event handlers have
  all been run BUT microtasks will be completed after each event handler if the
  JS stack is empty
    * the stack probably empties between handlers when responding to a real
      user click but not if the click is triggered by other code as that code
      will still be on the stack
    * the difference being
        * a real user click dispatches a event asynchronousely but
          `thing.click()` dispatches the event synchronously
* => microtasks are not necessairly run at the end of the current task - they
  are run anytime in the current task that the JS stack is empty
