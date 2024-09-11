# DynamoDB book

- [DynamoDB book](#dynamodb-book)
  - [Foreword](#foreword)
  - [What is DynamoDB?](#what-is-dynamodb)
  - [Core concepts](#core-concepts)
    - [Streams](#streams)
    - [TTL](#ttl)
    - [Partitions](#partitions)
    - [Write Request lifecycle](#write-request-lifecycle)
    - [Limits](#limits)
    - [Overloading keys and indexes](#overloading-keys-and-indexes)
  - [The three api action types](#the-three-api-action-types)
    - [Migrations](#migrations)
  - [Querying](#querying)
    - [PartiQL](#partiql)
  - [creating a table](#creating-a-table)
  - [ORM](#orm)
  - ['aws dynamodb' CLI](#aws-dynamodb-cli)
  - [Modeling](#modeling)
    - [Option: one table per model](#option-one-table-per-model)
    - [Option: single table design](#option-single-table-design)
    - [Designing primary key and secondary sort keys](#designing-primary-key-and-secondary-sort-keys)
        - [Option: `PK` and `SK`](#option-pk-and-sk)
        - [Option: choose a field](#option-choose-a-field)

## Foreword

> is talk on Advanced Design Patterns with DynamoDB by some guy named Rick Houlihan.

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

My takeaways

* The DynamoDB constraint is to put everything in one table
* Author believes this is actually better once you get over the "how do I model with this" hump because it requires much less compute so can scale much better than RDBMS

## What is DynamoDB?

* Author believes DynamoDB is superior to RDBMS in every way
* DynamoDB can handle 1:many and many:many relationships but it's different to how it works in a SQL DB
* Dynamo can scale
    * Amazon and AWS use DynamoDB for all their Tier1 (lose money if it goes down) services
* But you can design a DynamoDB that does not scale!
* Constraint: Unlike SQL DB, you must know you access patterns **before** you model with DynamoDB
* Dynamo does not **enforce** a schema but you do need one
* Dynamo won't enforce it so you need to do that in your application

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
* All requests are made via HTTP requests, not some other protocol like SQL DBs
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

> As Amazon scaled up their operations, they couldn't use costly operations
> like joins because they were too slow and resource-intensive. Werner shared that
> 70% of their database operations operated on a single record, and another 20%
> would return multiple rows but would use only a single table. Thus, over 90% of
> all queries did not use joins!

2012 AWS publish a paper on Dynamo https://www.dynamodbguide.com/the-dynamo-paper/

Indexing in DynamoDB is much more limited than other DBs

* no geospatial
* no full text search
* no location based
* multi-key indexes

> One benefit of DynamoDB is that its rigidity limits you in a good way. As
> long as you aren't using full-table scans, it's tough to write an inefficient
> query in DynamoDB. The data model and API restrict you into best practices
> upfront and ensure your database will scale with your application.

Cassandra is the closest in data model to DynamoDB - it is also a wide column store.

DynamoDB doesn't have any feature that would stop it scaling so it is missing
features but what is there is guaranteed to scale.

Unless you do full table scans, it is hard to write a slow query.

Eat your vegetables ...

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
        * requires you to provision additional throughput which is separate fro the read/write throughput for the core table
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
    * so you can observe all writes and process them with lambda functions
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

1. Item based actions
   * operates on specific item(s)
   * actions:
        1. `GetItem` - read single item
        1. `PutItem` - write single item
        1. `UpdateItem` - edit single item, create if doesn't exist (upsert)
       1. `DeleteItem` - delete single item from table
    * must be performed on main table, not global secondary indexes
    * must specify primary key
    * batch actions
        * do multiple single item actions in one request
        * each action succeeds/fails independently - failure of one won't stop the others
    * transaction actions
        * do multiple single item actions in one request
        * all actions succeed/fail as one - if one fails they all fail and cause the data to be rolled back
    * You must specify exactly which items you want to act on - you can't do `DELETE FROM things WHERE age > 10` kind of stuff
2. Queries
   * operates on item collection
   * retrieve multiple items with same partition key
   * can query on either main table for a global secondary index
3. Scans
   * operates on whole table

### Migrations

    TODO: read chaps 15,22 which are about migrations

Because it is schemaless, some kinds of migrations are really easy
    You can just add/omit fields from an existing records

You can evolve your data model over time - I wonder how easy that is compared to SQL?
    It's gotta be scans of the whole table and update each relevant record?
    Or maybe migrate to a whole new table if that's feasible
    Do I need to keep some sort of "version" string on each table? or implement migrations somehow?

Should table names include a version e.g. `users_123`, `users_124` to aid migrations?

I could mimic rails migrations by

* have a bunch of ruby scripts which perform a migration action as code - no DSL, just making changes
* keep a table in the DB to keep track of which scripts have run
* have a runner rake task which coordinates running them

What is a "migration" in Dynamo?

1. Scan the table, editing every row which needs editing (only the rows which correspond to the model you are migrating will need editing)
1. Create any new GSIs as required
1. Destroy any GSIs no longer required

Unless your code is able to work in the before, middle and after states of the migration then you will need downtime to achieve this

## Querying

> You must provide the name of the partition key attribute and a single value
> for that attribute. Query returns all items with that partition key value.
> Optionally, you can provide a sort key attribute and use a comparison operator
> to refine the search results.

The query takes a partition key and returns all records matching.
If you provide a sort key, it is applied as a filter expression to the matched records (you still read all the records matching partition key)

```sh
# pseudocode
query(part_key, sort_key=nil) => all records matching part_key and sort_key constraint
```

> Query results are always sorted by the sort key value. If the data type of the
> sort key is Number, the results are returned in numeric order; otherwise, the
> results are returned in order of UTF-8 bytes. By default, the sort order is
> ascending. To reverse the order, set the ScanIndexForward parameter to false.

> You can query a table, a local secondary index, or a global secondary index.

If the value is not in your partition key, sort key, local secondary index or global secondary index then you can't search for it!

Conceptually a DynamoDB query is much closer to a hash lookup than anything SQL ish.

I guess it's a bit like if SQL db didn't drop back to table scanning in the absence of an index

Q: can i scan for arbitrary values?

so when you query you have a "condition expression" where you put the partition and sort key stuff
and a "filter expression" where you put conditions targetting other fields in the record
so when you scan you have a "filter expression" where you put conditions targetting other fields in the record
> A filter expression cannot contain partition key or sort key attributes. You need to specify those attributes in the key condition expression, not the filter expression.

### PartiQL

A SQL like syntax but it still has all the constraints of DynamoDB

```sql
insert into eoin_1 value {'PK': '001', 'SK': '001', 'Artist' : 'Acme Band','SongTitle' : 'PartiQL Rocks'}
```

## creating a table

* table names must be unique in each region of your account

Q: can you create tables via the HTTP API or do you need to use the AWS API (via an SDK or Terraform)?

## ORM

* Presumably ORMs bake in some good data modelling ideas so they might be good as inspiration even if we don't use the code
* Book recommends against. But maybe it's ok for usages where you don't need mega scale?

## 'aws dynamodb' CLI

```bash
aws dynamodb
        batch-execute-statement
        batch-get-item
        batch-write-item
        create-backup
        create-global-table
        create-table
        delete-backup
        delete-item
        delete-table
        describe-backup
        describe-continuous-backups
        describe-contributor-insights
        describe-endpoints
        describe-export
        describe-global-table
        describe-global-table-settings
        describe-kinesis-streaming-destination
        describe-limits
        describe-table
        describe-table-replica-auto-scaling
        describe-time-to-live
        disable-kinesis-streaming-destination
        enable-kinesis-streaming-destination
        execute-statement
        execute-transaction
        export-table-to-point-in-time
        get-item
        help
        list-backups
        list-contributor-insights
        list-exports
        list-global-tables
        list-tables
        list-tags-of-resource
        put-item
        query
        restore-table-from-backup
        restore-table-to-point-in-time
        scan
        tag-resource
        transact-get-items
        transact-write-items
        untag-resource
        update-continuous-backups
        update-contributor-insights
        update-global-table
        update-global-table-settings
        update-item
        update-table
        update-table-replica-auto-scaling
        update-time-to-live
        wait
        wizard
```

## Modeling

This book strongly recommends using as few tables as possible.
ideally only a **single table per application/microservice**

The book supports the "Don't start on DynamoDB" idea and even suggests doing a
more normalised data approach (you still don't have joins tho)

> DynamoDB was built for large-scale, high-velocity applications that were
> outscaling the capabilities of relational databases. And relational databases
> can scale pretty darn far! If you're in the situation where you're out-scaling a
> relational database, you probably have a good sense of the access patterns you
> need. But if you're making a greenfield application at a startup, it's unlikely
> you absolutely require the scaling capabilities of DynamoDB to start, and you
> may not know how your application will evolve over time.
>
> In this situation, you may decide that the performance characteristics of a
> single-table design are not worth the loss of flexibility and more difficult
> analytics. You may opt for a Faux-SQL approach where you use DynamoDB but in a
> relational way by normalizing your data across multiple tables.
>
> This means you may need to make multiple, serial calls to DynamoDB to satisfy
> your access patterns.

1. Create an ERD diagram (or similar understanding) of your app
1. Write down all your access patterns, both primary and secondary
1. Model your primary key structure to satisfy your primary access patterns
1. Satisfy additional access patterns with secondary indexes and global secondary indexes
2. Expect to have to iterate on this.

Principles

1. Consider what your client will know at read time
   * The client must know the primary (and secondary if exists) keys
2. Use primary prefixes to distinguish between entity types in the same table
   * For example, a single table might have keys fo the form:
        ```
        PK              SK
        ----------------------------
        USER#213        METADATA#213
        GROUP#123       METADATA#123
        ROLE#123        METADATA#123
        BOOKING#123     METADATA#123
        ```
3. Handle additional access patterns with secondary and global secondary indexes
    * ideally your primary+secondary key will cover most of your use cases
    * additional indexes do cost more

Q: How do I iterate on the design of an in-use DynamoDB table?
    Hope it's not so big that I can't just create a new table and copy all the data across

Q: Would it make sense to have my own model layer which captures each access pattern
    this could then call down into a lower layer which actually fetches the data

* DynamoDB requires more design up front than a SQL DB.
* You need to know your access patterns **at design time** unlike with a SQL DB
    * Because that's always possible ...

Workbench facets

* a facet is a way of looking at just a single type of object within a single table design
    * they aren't called "views" because that overlaps with the meaning in a SQL DB

### Option: one table per model

you can use one table per model
* -- less optimized than the denormalised
* -- you have to do the joins in application memory (no RDBMS to join for you)
    -- more network requests than with a SQL DB
* ?? Could this be better if you don't yet understand your traffic patterns?

### Option: single table design

* ++ more optimized
* recommended by this book
* -- harder to evolve the design as requirements change
* Keep data attributes and indexing attributes separate
* be ok with seeming duplication between data attributes and indexing attributes
* remember that you can query on indexed attributes but not data attributes
    * caveat: you can scan for anything but it's very slow

> “For each global secondary index you use, give it a generic name of
> GSI<Number>. Then, use GSI<Number>PK and GSI<Number>SK for your attribute types”

Add a `Type` field to your data attributes to help map back to a model in your code
    * lets you filter your scans

> DynamoDB is not great at performing ad-hoc OLAP-style queries, so you will
> probably import your data to Amazon Redshift or export it to Amazon S3 to query
> with Amazon Athena

Well that sentence does a lot of heavy lifting.

It seems likely that an app would need a solution outside of DynamoDB to handle reporting, support etc. (the stuff a SQL DB is good at)

Common options are to stream DynamoDB into

* RDS
* S3 + Athena
* Redshift


### Designing primary key and secondary sort keys

##### Option: `PK` and `SK`

* Name the primary key `PK` and the secondary sort key `SK`
  * the most abstract
  * whatever bits of data make up your PK and SK will be be duplicated in your data record
*

##### Option: choose a field