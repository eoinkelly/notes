# Event loop

Sources

* https://www.youtube.com/watch?v=cCOL7MC4Pl0

Overview

* JS runs on the "main thread" as does rendering, interaction etc.
* Browser has many threads under the hood but they all come back to the main thread with their information


* Tasks
    * are what the event loop runs
    * style calculations, layout and paint are also tasks on the event loop
      * => your JS task will finish to completion before next render happens i.e. you won't get renders in the middle of your JS
      * `setTimeout(() => {}, 0)` will cause the function to be added to the task queue - it will definitely not run in this turn of the event loop
        * in practice the soonest a browser can run the next task is approx 4.7ms so we can ask for 0 but never really get it
    * `requestAnimationFrame` creates a new task but doesn't put it on the normal "run as soon as you can" task queue. It puts it on a queue where it is run when the browser decides to update the screen
* micro tasks
    * a separate queue
    * they are run when in RAF, when the last task on the main task queue finishes
    * promise callbacks are executed on the microtask queue
* event loop

## Queues

* Task queue
  * runs one task only on each turn
* Animation callbacks queue
  * runs all tasks which were on the queue when it started executing
  * tasks which turn up after it started executing will run before the next frame
* Microtasks queue
    * when execution starts, the queue is processed to completion **including any new items which turn up during execution**
        * => you can block rendering by keep adding stuff to the microtask queue


User clicking a link and clicking the link in JS via `click()` behave differently wrt microtasks! - see talk above for details.