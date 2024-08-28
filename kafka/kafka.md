# Kafka

## Tools

-   kcat
    ```sh
    # kcat (rust, from homebrew, does what you expect)
    kcat -b localhost:9092 -L
    ```
-   Kadeck
    -   GUI app, bit fiddly but ok, some features require paid upgrade
-   Kafka Magic
    -   Runs locally but runs in background and opens browser for GUI
    -   -- fiddly to quit
    -   UI is basic. Kadeck is slightly better
-   Kafka-ui
    -   web based, runs in docker
    -   best I've found so far

## Background

-   a **distributed** event streaming platform
-   runs on JVM
-   uses
    -   data pipelines,
    -   streaming analytics,
    -   data integration
-   distributed, highly scalable, elastic, fault-tolerant, and secure

1. To publish (write) and subscribe to (read) streams of events, including continuous import/export
   of your data from other systems.
1. To store streams of events durably and reliably for as long as you want.
1. To process streams of events as they occur or retrospectively.

-   client server model
-   communicates with it's own protocol over TCP
-   servers run in a cluster
-   clusters can span data centers or cloud regions
-   server roles
    1. Broker servers
        - the storage layer
    2. Kafka Connect servers
        - continuously import/export data as event streams
        - each connector runs as a separate process

Record streams are persistent in Kafka as Kafka acts as a record store. Kafka does not remove
records once consumers process them. You rather configure record retention periods. When the record
retention period expires, Kafka removes the records.

## Partitions

Topics can be split into partitions for improved scalability and throughput. Each partition is a
single log file where Kafka writes records in an append-only fashion.

## Consumer groups

-   each `groupId` should only be used with a single application
-   Consumers can self identify as being part of a "group"
-   Admin can describe the group to see membership

## Confluent

https://www.confluent.io/apache-kafka-vs-confluent/

They package up OSS Kafka into a cloud offering

## Events

-   also called records or messages

Schema registry ??

## Clients

https://kafka.js.org/docs

-   Kafka ships with clients for some languages, others are community provided
-   There are multiple kinds of clients:
    1. Producer client
    2. Consumer client
    3. Admin client
-   Most (all?) Kafka libs include both

The official Kafka download comes with shell scripts to do basic admin

Topics Each event has a topic Consumers can subscribe to a topic

Consumers have a group ?? messages have a topic and a partition ??

There are many ways to interact with Kafka

1. Use the producers/consumers API to created and read messages and topics
2. Use the Steams API to transform messages as they pass through Kafka
3. Use Kafka Connect

Internally messages are untyped byte steams

## Transactions

If you send messages within a transaction then a "transactionally aware" consumer will only read
messages which are committed

Note: Kafka requires that the transactional producer have the following configuration to guarantee
EoS ("Exactly-once-semantics"):

-   The producer must have a max in flight requests of 1
-   The producer must wait for acknowledgement from all replicas (acks=-1)
-   The producer must have unlimited retries

## Kafka-Connect

Kafka Connect is a framework and toolset for building and running data pipelines between Apache
Kafka® and other data systems. It provides a scalable and reliable way to move data in and out of
Kafka.

Kafka Connect is a tool for connecting different input and output systems to Kafka. Think of it like
an engine that can run a number of different components, which can stream Kafka messages into
databases, Lambda functions, S3 buckets, or apps like Elasticsearch or Snowflake.

Good for bulk IO

List of connectors: https://docs.confluent.io/platform/current/connect/supported.html

## Kafka Streams API

-   Java only API - you must use a JVM language (Java/Scala etc.) to use the streams API
-   A higher level of abstraction than using the producer/consumer API
-   Use it when you would have to write too much boilerplate code using the producer/consumer API
-   Can do stateful or stateless transformations
-   Streams implements exactly once processing semantics
-   has flexible windowing support
-   no separate processing cluster required - the streams code runs inside your application
-   seems to be some effort to have an equivalent for JS:
    https://www.npmjs.com/package/kafka-streams (warning: this is not used a lot)

Kafka Streams is another project from the Apache Kafka community. It is a Java API for processing
and transforming data **inside** Kafka topics.

Kafka Streams, or the Streams API, makes it easier to transform or filter data from one Kafka topic
and publish it to another Kafka topic, although you can use Streams for sending events to external
systems if you wish. (But, for doing that, you might find it easier to use Kafka Connect)

You can think of Kafka Streams as a Java-based toolkit that lets you change and modify messages in
Kafka in real time, before the messages reach your external consumers.

To use Kafka Streams, you first import it into your Java application as a library (JAR file). The
library gives you the Kafka Streams Java API.

With the API, you can write code to process or transform individual messages, one-by-one, and then
publish those modified messages to a new Kafka topic, or to an external system.

With Kafka Streams, all your stream processing takes place inside your app, not on the brokers. You
can even run multiple instances of your Kafka Streams-based application if you’ve got a firehose of
messages and you need to handle high volumes.

Stream processing frameworks like Kafka Streams or Apache Flink offer several key features that
enable real-time data processing and analytics:

1. State Management: Stream processing systems can manage state across data streams, allowing for
   complex event processing and aggregation over time.
2. Windowing: They support processing data in windows, which can be based on time, data size, or
   other criteria, enabling temporal data analysis.
3. Exactly-once Processing: Advanced systems provide guarantees for exactly-once processing
   semantics, ensuring data is processed once and only once, even in the event of failures.
4. Integration with External Systems: They offer connectors for integrating with various data
   sources and sinks, including databases, message queues, and file systems.
5. Event Time Processing: They can handle out-of-order data based on the time events actually
   occurred, not just when they are processed.

Stream processing frameworks are NOT available for most programming languages, including JavaScript.
Therefore, if you live in the JavaScript world, you have three options:

1. Build all the stream processing capabilities by yourself. Trade-off: A lot of work!
1. Leverage a stream processing framework in SQL (or another programming language): Trade-off: This
   is not JavaScript!
1. Don’t do stream processing and stay with APIs and databases. Trade-off: Cannot solve many
   innovative use cases.
