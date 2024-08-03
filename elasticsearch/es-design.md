## Decisions you make when you create an index

## Sources

-   Elasticsearch in Action (but out of date but very good IMHO)
-   Official ES 7.17 manual

## Designing a cluster

-   Keep shard size in the 10-50 GB range for best performance
    -   => Don't shard for less than 10GB


Sources

-   https://thoughts.t37.net/designing-the-perfect-elasticsearch-cluster-the-almost-definitive-guide-e614eabc1a87 (this is very good)
-   https://www.elastic.co/blog/how-many-shards-should-i-have-in-my-elasticsearch-cluster

Overview

-   Decisions you can change later
    -   Choose number of nodes in cluster
    -   Choose number of replica shards for each primary shard
-   Decisions you cannot change later
    -   Choose number of primary shards for an index based on your estimate of
        1. index size
        2. index usage
-   Your first design will probably suck because you don't know what the workload will be
    -   => you will have to iterate a few times
    -   questions you won't yet know the answers to
        -   how many queries/sec?
        -   does your cluster's workload need a lot of CPU, memory or both?
            -   influences whether to go with a few large hosts or more smaller hosts
        -   what kinds of queries?
        -   how fast do indexes grow?
-   You should have an odd number of nodes to avoid split-brain
-   lucene creates lots of small files - choose a file system which can handle this - inodes etc.
-   during a merge lucene makes copies of all segments before changing them so your shard can't be more than 0.5 your free disk space
    -   a merge is when lucene consolidates two smaller segments into one
-   shard size
    -   50GB shard should be your max (it's not an enforced max)
    -   don't make them too small because small shards have many small segment files
    -   aim for shards in the 20GB - 50GB range
-   ES can scale to many, many nodes

Advice from the t37.net blog post

> less 3M documents: 1 shard
> between 3M and 5M documents with an expected growth over 5M: 2 shards.
> More than 5M: int (number of expected documents / 5M +1)

* ES started as a horizontally scalable Lucene
  * https://lucene.apache.org/
* Original use-case was search but it has powerful aggregation features especially when combined with Kibana as a workbench
* It aims to compete with more heavyweight Flink and Spark
  * https://flink.apache.org/
  * https://spark.apache.org/
* Each shard is an inverted index of documents

* ES 8
  * can handle endless real-time data streams via it's Data Stream feature
  * it can ingest serverless logs from AWS, Azure
  * Can import pytorch models to do NLP
  * tighter security by default
  * Experimental features:
    * Vector similarity/kNN search
    * More ML stuff
  * New canvas editor
  * Maps/vector-tile support
  * New Kibana UI


You can easily add read capacity (which is determined by replicas) but not easily add write capacity (which is determined by primary shards)

When you update a doc in elasticsearch

1. it creates a new document with an incremented version number
1. The old document is marked for deletion (but won't actually be deleted until later)


## Overview

| Relational DB | Elasticsearch     |
| ------------- | ----------------- |
| Database      | The whole cluster |
| Table         | Index             |
| Row           | Document          |
| Column        | Field             |

-   A node is an instance of ES
-   node exposes Restful JSON API on port 9200
-   nodes form a cluster on port 9300
-   ES is a document database
    -   documents have fields
-   by default every field in a document is indexed in an inverted index i.e. it is searchable
-   uses JSON as the serialization format for documents
-   storing a document in ES is called "indexing the document"
-   A cluster is a group of nodes with the same value of `cluster_name` (you can see this value by visiting in a browser http://localhost:9200/)
-   Provides a **Java API** and a **HTTP API**
-   Supports YAML in request and response bodies - add `?format=yaml` to your request
-   You can configure your index settings to not store `_source` (the original indexed document)
    -   Doing this in combination with using an external ID means you could use ES just for the searching of the index and get a list of database IDs as results.
    -   You would then get those rows from the DB
    -   Is this a good pattern?
        -   ++ keeps the index size smaller (which only matters if you have very large source documents)
        -   -- you have to do more work to get results
-   A lot of the constraints of ES are actually constraints from Lucene
-   The part of ES that writes data to disk is called the "gateway"
-   ES is described as "schema free" or "schemaless"
    -   It's kinda bullshit
    -   That means the docs are not _bound_ by a schema, it doesn't mean there is no schema
-   **near** real-time not real-time search

### Default ports

-   TCP 9200 for Restful API queries
-   TCP 9300 for inter-node communication or "transport"
    -   The Java API connects to this port
-   TCP 5601 for Kibana
    -   Kibana connects to ES server over 9200

### Things it isn't good at

-   ES does not support transactions - you should use something else if you need them
-   ES doesn't work as well as a store if you have many frequent writes
-   ES doesn't do great at modelling data with lots of complex relationships

### Elasticstack

-   Elasticsearch
    -   Search API based on Lucene
    -   Features
        -   documented oriented (schemaless) DB
        -   near real time
        -   distributed
        -   search and analytics engine
        -   categorised under NoSQL
-   Kibana
    -   Web UI for elasticsearch
    -   provides management and dev tools for elasticsearch
    -   lets you build visualisations
    -   http://localhost:5601
-   X-pack
    -   provides
        -   monitoring,
        -   reporting
        -   alerting,
            -   send alerts to slack etc. based on the result of some search e.g. too many failed login attempts
        -   security,
            -   provides authentication and authorization to your cluster or documents and even fields within it
        -   graphing
            -   visualise your data as a connected graph rather than a flat set of data files in documents
            -   includes a _Graph API_ and some UI within Kibana
        -   machine learning
            -   finds anomalies in time series data (unsupervised learning)
    -   prior to 6.3 it required registration and downloading separate code for some features. After 6.3 you get all code when you download elasticsearch but some parts require commerical license
-   Logstash
    -   centralised daemon which has many plugins to allow ingesting logs from various places, filtering them, transforming them and sending them to various places including elasticsearch
    -   has 200+ plugins
-   Beats
    -   a collection of components built on top of a core `libbeat`
    -   capable of collecting files from disk, metrics from OS, metrics from specific binaries
    -   the client-side to logstash's server-side
    -   installed on machines which generate the logs
-   Elastic cloud
    -   fully hosted Elastic stack provided by elastic.co
