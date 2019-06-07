## Data models

* The data models you choose shape the solution **a lot**

Q: where do column stores fit in these data models?

## Relational model

* SQL is the best known example
* Data is organised into relations (aka tables)
* Each relation is an **unordered** collection of tuples (aka rows)
* Other DBs at the time (early 80's) forced users to think a lot about how the data was stored. Relational model hid all that details
* Relational model started life in "business processing" but has generalised well over the years to many other areas.
* The relational model allows you to use declarative queries (saying what you want to happen) and then the query optimizer decides _how_ to do it.
* Having a query optimizer is a key part of the relational model. It is a complex piece of software but (so the story goes) only has to be built once.
* The query optimizer effectively chooses the "access path" (see network model below) each time.

Relational databases struggle when

* there is a very high write throughput
* if users want a more dynamic data model which isn't restricted by schemas
* if the data model has a lot of many-many relationships then the relational model becomes cumbersome to use

Most modern RDBMS support some sort of multi-valued data in a row e.g. XML or JSON content which allows them to behave somewhat like document databases

## Heirarchical model

* Originally designed for IBM _Information Management System_ (1968)
* Represented data as a tree of records nested within records
* ++ good at one-many relationships
* -- many-many relationships were difficult because the DB didn't support joins
* every record had exactly one parent

## Network (aka CODASYL) model

* A generalisation of heirarchical databases
* Proposed as a way to fix some limitations with the heirarchical model
* Standardised by the _Conference on Data Systems Languages_ CODASYL
* each record could have multiple parents
    * this allowed many-one and many-many relationships to be modelled
* the links between records were not foreign keys but more like C pointers
* the only way to access a record was to follow these pointers from a root record - this path was called an _access path_
* A query was performed by moving a cursor though the DB by iterating over lists of records and following access paths
* ++ this was efficent when storage was really slow (e.g. tape drives)
* -- it was very hard to change a data model because the knowledge of the access paths was encoded in so many places in a codebase


## Document model

Note that we are talking about the _data model_ here only, not the other properties of a DBMS e.g. fault tolerence, concurrency

* -- a write to any part of the document forces a full rewrite of the whole document
    * => avoid writes which edit a document
* -- a read of any part of a document forces the whole document to be read
    * Avoid for use cases where you regularly need only part of a document
* ++ is good at representing one-many relationships (document can have other documents direclty embedded or linked to)
* -- is bad at representing many-one or many-many relationships
    * if the DB does not support joins then you have to do it in application code
    * data tends to become more connected over time so there is a risk of new relationships appearing over time i.e. a document DB might become less of a good choice over time.

Q: do modern document DBs support many-one or many-many in special ways i.e. do they support joins?


Databases using the document model

1. CouchDB
1. RethinkDB
1. MongoDB
1. Espresso

### Data storage locality

Docuent DBs have (in general) good storage locality. If the application needs to access a whole document regularly then they are efficient because the whole document is stored contigiously on disk (unlike in the relational model which will have to pull data from multiple tables and indexes)

Some relational databases do have good storage locality

* Google Spanner is a relational DB which has good document locality
    * It allows a table to indicate that its rows should be stored interleaved with a parent table
* Oracle has a similar feature called _multi table index cluster tables_
* Cassandra (and Hbase) have the concept of a _column family_ which is simlar

## Graph model
