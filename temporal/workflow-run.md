
A workflow run is identified by `(workflowid, runid)` and these "runs" are what Temporal UI calls "workflows"

- The run is a sequential event history
- Each event is numbered starting at 1
- There are a constrained set of event types


Common event types

- These are always the first and last events in a completed workflow:
    - Workflow Execution Started
    - Workflow Execution Completed

Workflow tasks have 3 events which appear in order

1. Workflow Task Scheduled
    - Notes that the server has put a task on a particular queue
    - The server creates a special temporary queue that represents a particular worker - later tasks on the same workflow are put on that queue rather than the general one if TASK_QUEUE_KIND_STICKY is set
        - Presumably this is a performance optimisation because that worker already has that workflow suspended at the right point in memory so can pick up easier
1. Workflow Task Started
    - Notes that a worker has picked up the task (worker details are in event)
1. Workflow Task Completed
    - Notes that a worker has completed the task
        - No return value from a task
        - Details about the worker which completed the task are in the event

Activity tasks have 3 events which appear in order

1. Activity Task Scheduled
    - Notes that a particular activity task has been put on a queue
    - Input to the task is in the event
1. Activity Task Started
    - Notes that the task has been picked up by a worker
        - Presumably between this and completed implies the worker is working on it
    - Input to the task is in the event
1. Activity Task Completed
    - Input and result of task are in the event
    - Notes that the activity has been completed


When a workflow is started, the first thing that happens is the server puts the first task in that workflow onto the appropriate task queue

Activity tasks and workflow tasks can live on the same task queue


## State transitions

Why would a history with 11 events list 7 state transitions? which events are ignored?


## Mental model

My guess at how it works: Temporal doesn't introspect workflow code ahead of time to break it into tasks.
When Temporal picks up your workflow code, it runs it until it encounters a "task ending" scenario, one of:

1. throwing an error
2. starting an activity
3. finishing the workflow code
4. ?? start child workflow (probably?)
5. ?? others

When it hits this scenario, it suspends the workflow code and schedules the next thing if there is something to schedule.

?? does it actually suspend the code? it seems like it wouldn't need to - could just replay the history. but it might actually suspend and keep in memory as perf optimisation???

When the worker gets a task again, it either

1. reruns the workflow from the start, plugging in values for activity returns it knows about. Runs the workflow until it gets to the next "task ending scenario"
2. uses the execution still in memory from the last time and just "returns" the value from the activity and continues execution.

Temporal does not keep the workflow execution suspended in memory but it does cache in memory the history of the workflow if Sticky Cache (Performance Optimization) is enabled. This allows it to re-run the workflow from the start but have all the events in memory already so it's faster

 To avoid the overhead of replaying the entire history for every task, Temporal workers maintain a "sticky cache." This cache stores the state of recently active workflows in memory. If a subsequent task for a cached workflow arrives, the worker can simply "resume" execution from the cached state, significantly improving performance.


Key idea: Temporal preserves **enough** state to recreate the complete state of the workflow at any point.


Your workflow code will be run a bunch of times
Every time it does something to end a "task", then it will be re-run from the start again

### A Task is a run of the workflow code from the start

SO it's not correct to think of a "Task" as a chunk of workflow code
A task is a full re-run of the workflow code from the start, filling in state from history until it hits some new "task ending scenario" which causes the "attempt" to end and some new thing to be scheduled
    it's another "attempt" at the workflow


Workflow Task = "Re-run the workflow from start until you hit the next thing that requires stopping"


## Nuance: Timers are started on the Temporal server not any worker

Timers are scheduled like an activity and the worker stops. The timer exists on the server only!