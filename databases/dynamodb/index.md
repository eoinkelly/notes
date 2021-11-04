# DynamoDB book

(500ish pages)

## Foreword

> ia talk on Advanced Design Patterns with DynamoDB by some guy named Rick Houlihan.

Good links on https://www.dynamodbguide.com/additional-reading/

Author says that RDBMS created in world where storage very expensive to compute
- storing data normalised saves storage at the cost of compute required to
de-normalise and create as reports but that now it has flipped and storage is
much cheaper than compute.

    That seems to ignore the added complexity of storing duplicate data?

Author seems to equate NoSQL with DynamoDB

> At the core of all NoSQL databases there is a collection of disparate objects,
> tied together by indexed common attributes, and queried with conditional select
> statements to produce result sets. NoSQL does not join data across tables, it
> effectively achieves the same result by storing everything in one table and
> using indexes to group items.

Takeaways

* The DynamoDB constraint is to put everything in one table
* Author believes this is actually better once you get over the "how do I model with this" hump because it requires much less compute so can scale much better than RDBMS

## What is DynamoDB?

* Author believes DynamoDB is superior to RDBMS in every way
* DynamoDB can handle 1:many and many:many relationships but it's different to how it works in a SQL DB
* Dynamo can scale
    * Amazon and AWS use DynamoDB for all their Tier1 (lose money if it goes down) services
* But you can design a DynamoDB that does not scale!


Constraint: Unlike SQL DB, you must know you access patterns **before** you model with DynamoDB
You can evolve your data model over time
    I wonder how easy that is compared to SQL?

Dynamo does not **enforce** a schema but you do need one
Dynamo won't enforce it so you need to do that in your application

DynamoDB is

* key value store (conceptually a giant hash table)
    * fetch one record at a time
* wide column store
    * every **value** in the key-value store is a b-tree
        * so getting/setting has the constraints of b-tree e.g.
          * can match exact
          * can match prefix
          * can do range queries ("all values between X and Y")
* fully managed
* infinite scaling with no perf decline
    * limited by your wallet
    * no technical limit on how big a table can be
    * perf does not decline as your table grows - your 100TB table will be as fast as your 1GB table
* most responses in 1-9 mS range
    * Can make this faster with DynamoDB Accelerator (DAX) is a fully manged in-memory cache for a single DynamoDB table
* HTTP connection model
* All requests are mad via HTTP requests, not some other protocol like SQL DBs
* Connections are not persistent unlike in SQL DBs
    * This makes DynamoDB slower than RDBMS for some use-cases because you have to setup the connection every time
    * But it does enable the huge scaling

* Uses IAM for authentication and authorization - no usernames and passwords
    * You can use roles if your compute is also in AWS
    * Can get quite granular in IAM about what actions a user can perform on a table and
    * what primary keys they can see/edit
    * what attributes they can see on those allowed keys
* Allows/forces you to create tables etc. with your CloudFormation/Terraform
* Priced
    * On-demand pricing
      * Pay per request
      * costs more han provisioned pricing
    * Provisioned pricing, based on
        * _Read capacity unit_
            * One strongly consistent read (up to 4kb) per sec
            * OR
            * Two eventually consistent reads (up to 4kb) per sec
        * _Write capacity unit_
            * write one item up to 1KB in size) per second
    * ++ you can tweak read and write separately to suit your workflow
    * You can start _On demand_ and switch to provisioned when you learn your traffic patterns.
    * ?? says you can scale RCU and WCUs at nights and weekends, how does that work? manually or can be run on a schedule?

Q: how is to use from things outside AWS?

> “As Amazon scaled up their operations, they couldn’t use costly operations
> like joins because they were too slow and resource-intensive. Werner shared that
> 70% of their database operations operated on a single record, and another 20%
> would return multiple rows but would use only a single table. Thus, over 90% of
> all queries did not use joins!”

2012 AWS publish a paper on Dynamo https://www.dynamodbguide.com/the-dynamo-paper/


Indexing in Dynamo is much more limited than other DBs

* no geospatial
* no full text search
* no location based
* multi-key indexes


> “One benefit of DynamoDB is that its rigidity limits you in a good way. As
> long as you aren’t using full-table scans, it’s tough to write an inefficient
> query in DynamoDB. The data model and API restrict you into best practices
> upfront and ensure your database will scale with your application.”

Eat your vegetables ...

Cassandra is the closest in data model to DynamoDB - it is also a wide column store

DynamoDB doesn't have any feature that would stop it scaling so it is missing features but what is there is guaranteed to scale.

Unless you do full table scans, it is hard to write a slow query.

## Core concepts

1. Tables
    * a grouping of records
    * somewhat comparable to a table in a SQL DB
    * often contains multiple types of entity (unlike a SQL DB)
      * avoids a JOIN operation
    * schema-less - you don't **declare** a schema and Dynamo won't **enforce** one (but you still need one!)
1. Items
    * a single record
    * comparable to a row in a SQL DB
1. Attributes
    * each item contains attributes
    * somewhat akin to column names in SQL DB
    * a name and a **typed** value
    * 10 Data types
        * Scalars
            1. string
            2. number
            3. binary
            4. boolean
            5. null
        * Complex
            * compound types
            * represent groupings with arbitrarily nested attributes
            * types
            1. list
            2. map
        * Sets
            * compound type
            * every element must be same type
            * duplicates are not stored
            * types:
                1. string set
                2. number set
                3. binary set
1. Primary keys
    * two types:
       1. simple
          * single value called a "partition key" (sometimes called "hash key")
          * lets you fetch a single item
       2. composite
          * two values: "partition key" and "sort key" (sometimes called "hash key" and "range key")
          * lets you fetch multiple items and then narrow the result set based on the sort key
          * enables "fetch many"
    * every item in table must include the primary key
    * writes which don't have primary key are rejected
    * write with an existing primary key will overwrite or be rejected (depending on option you set)
    * **primary key selection and design is the most important part of modeling with DynamoDB**
2. Secondary indexes
    * you specify primary keys for your secondary index and dynamo will **copy** all items from your main table to the secondary index in the reshaped form
    * When creating a secondary index you specify a key schema (similar to a primary key for a table)
        * can be simple or composite (see above)
    * two types
      1. local secondary index
        * uses same partition key as your primary key but a different sort key
            * useful if you are often wanting all the values for a particular partition key value but you want to filter/sort them differently than the primary key "sort key" would allow
            * acts like filters on the primary key "partition key"
        * can use the throughput from the core table
        * you can opt for strongly consistent reads or eventual consistent reads (strong costs more throughput)
        * -- must be created when table is created
        * -- you cannot delete them without deleting the whole table!
        * -- there is an item collection size limit
      2. global secondary index (**preferred option in most cases**)
        * you choose any attributes you want for partition key and sort key
        * used much more frequently because they are more flexible than local secondary indexes
        * requires you to provision additional throughput which is separate fro the read/write throughput ofr the core table
        * only offers eventual consistency (data is replicated from the core table to global secondary indexes in an async manner)
        * ++ you don't need to add them at table creation time
        * ++ you can delete them if you need to
    * you can then run queries against the secondary index
    * ??? Is a secondary index just a reshaped copy of the original table?
    * ??? i guess this makes them expensive for writes? and for storage?
3. Item collections
    * all items in a given partition key, both in main table and any local secondary indexes
    * a group of items which share the same partition key in either the base table or a secondary index
    * Dynamo partitions data across nodes to allow scaling
    * all items with the same partition key are stored on the same node
    * a lot of data modeling is based on creating proper item collections

### Streams

* a stream is an immutable sequence of records which can be processed by multiple independent consumers
* you get a transactional log of each write transaction to the table
    * so you can observe all writes and process them (how???)
* use cases
    * update record in another DynamoDB table
    * update record in another data store
    * trigger an event when a particular item (or items) change
    * audit and archive data
    * replicate data across multiple tables (similar to materialized views)
    * you could even push all data into a SQL DB for later report generation
    * analytics
* how
    * common
        * lambda
        * app that uses the Kinesis client library with the streams kinesis adapter


### TTL

* Dynamo can automatically delete items after a time-out
* how it works
    1. you specify which attribute is the timestamp for item deletion (store as unix timestamp as `number` type)
    2. Dynamo will periodically review the table and delete items which are old enough
* you don't have to use it for all items in your table
* exact deletion time is not promised - items could persist up to 48 hours after TTL expires

### Partitions

* the core storage units underlying a Dynamo table
* data is sharded across servers
* data is written to multiple nodes under the hood to ensure you don't lose it if a single node dies
* Each partition has 3 nodes (1 primary, 2 secondary)
* Secondary nodes can serve read requests
* In the past you had to care more about partitions because throughput was not spread across them but now throughput is automatically spread across your table to the items that need it (called "adaptive capacity")
* Every item collection is within a single partition

### Write Request lifecycle

1. Request router with authenticate and authorize request
1. Request router look at partition key in request and applies hash function to it.
1. Hash function output indicates which server the data is stored on
1. Primary node commits the write
1. Primary node sends write to one of two secondary nodes which commits it
1. Primary node sends response to client
1. If an eventually consistent read request comes in now and hits the other secondary node then it will get old data
   1. A strongly consistent read will not go to the secondary node with old data (somehow ???)
2. Primary node sends write to other secondary node async

### Limits

1. single item limited to 400 KB
    * be careful if you try to store a one to many relationship in a single item
1. `Query` and `Scan` will read max 1MB from table.
    * limit applied **before** filter expressions are considered
    * => you will need to paginate more often
1. Single partition  can have a max of 3000 RCU or 1000 WCU
    * capacity units re on a per-second basis
    * limit applies to a single partition, not a table as a whole
    * you need to be doing 3000 reads/sec for given partition key to hit these limits
1. If you have a **local** secondary index, a single item collection cannot be larger than 10GB
    * If you have a data model with many items with the same partition key this could mean your writes start getting rejected because you run out of partition space
    * this is not a problem for **global** secondary indexes

### Overloading keys and indexes

Sometimes the best names for your partition key and sort key attributes is a generic "PK" and "SK" (this is called "overloading" the keys)

Model a 1-many

| PK            | SK              | Notes                       |
| ------------- | --------------- | --------------------------- |
| ORG#{OrgName} | ORG#{OrgName}   | the actual org record       |
| ORG#{OrgName} | USER#{UserName} | record of a user within org |
| ORG#{OrgName} | USER#{UserName} | record of a user within org |
| ORG#{OrgName} | USER#{UserName} | record of a user within org |

## The three api action types