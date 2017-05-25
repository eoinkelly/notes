
Generic stage

* a high level abstraction over erlang processes for managing concurrent execution
* three types of stage
    1. producer
    1. consumer
    1. producer-consumer

stages exchange "events"
your stages are wired into a pipeline
GenStage has built-in back pressure
    so if one stage is slow the backpressure will slow/stop the production of events
    has a way to signal betwene processes about their businesses
stages are demand driven

1. consumer subscribes to a producer
2. consumer tells producer how many event it is willing to accept
3. producer sends at most that many events

consumer can send new messages to the producer about how many events it can now handle => consumption is demand driven
each stage runs in its own process so each stage is running concurrently
GenStage is essentially just a message contract or protocol

Consumers have configuration options
    max_demand - how much to ask for in any one message
    min_demand - how much to have left to process before asking for more
