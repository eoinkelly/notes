# Apache Spark

https://Spark.apache.org/

## Unanswered questions

    How do Delta Lake tables map into a Spark data frame?
    ? are sql queries sent to Spark converted to bytecode?
    ? how does DBFS relate to spark - it's part of the Databricks vm

## Install

> [!WARNING]
> pySpark fails on python 3.12 as of 2024-03-27, works on 3.11, 3.10

```sh

# Decide which language you are going to run Spark from

# python option 1
$ docker run -it --rm Spark:python3 /opt/Spark/bin/pySpark

# Python option 2
$ pip install pySpark
$ pySpark

# SQL
$ docker run -it --rm Spark /opt/Spark/bin/Spark-sql

# Scala
$ docker run -it --rm Spark /opt/Spark/bin/Spark-shell

# Java
$ docker run -it --rm Spark /opt/Spark/bin/Spark-shell

# R
$ docker run -it --rm Spark:r /opt/Spark/bin/SparkR
```

## Overview

-   developed at UC Berkeley in 2009.
-   team that started the Spark research project at UC Berkeley founded Databricks in 2013
-   supported languages
    1. Python
    1. SQL
    1. Scala
    1. Java
    1. R
-   Spark itself is written in Scala
    -   it ships a large collection of `.jar` files (323MB total) - see appendix below
-   what it does
    -   execute distributed ANSI SQL queries - **Spark includes a SQL engine**
    -   provides common machine learning models
    -   can do both batch and real-time streaming
-   Spark-sql loads a Spark SQL CLI driver class `org.apache.Spark.sql.hive.thriftserver.SparkSQLCLIDriver`
-   performance tuning
    -   Spark performance tuning is Java performance tuning
    -   https://Spark.apache.org/docs/latest/tuning.html
-   The idea of moving the code to where the data is (vs other way around) is key (see below for why S3 is still better than doing this locality with HDFS)
-   History
    -   Started with Google File system + MapReduce (Google)
    -   Hadoop File System (Yahoo) - used the GFS paper but open source
        -   Includes: Hadoop COmmon, MapReduce, HDFS, Apache Hadoop YARN
        -   Hadoop companies: Cloudera and Hortonworks
        -   -- hard to admin
        -   -- brittle fault tolerance
        -   -- slow
        -   Many other systems spawned to fix the problems with Hadoop
            -   Examples: Hive, Storm, Impala, Giraph, Drill, Mahout
                -   -- each had their own cluster configuration
            -   -- added to the complexity of Hadoop
        -   Spark started at UC Berkeley to make Hadoop MapReduce faster and fix other Hadoop issues
        -   > The central thrust of the Spark project was to bring in ideas
            > borrowed from Hadoop MapReduce, but to enhance the system: make it
            > highly fault tolerant and embarrassingly parallel, support in-memory
            > storage for intermediate results between iterative and interactive map
            > and reduce computations, offer easy and composable APIs in multiple
            > languages as a programming model, and support other workloads in a
            > unified manner.
        -   Databricks is _the_ Spark company - they were there before 1.0
-   features
    -   unified engine for large scale distributed data processing
        -   Spark wants to be the one engine to rule them all, unify all use-cases in a single core engine with libs on top for the use-case
    -   in memory storage for intermediate computations (makes it faster than Hadoop MR)
-   philosophical pillars
    1. Speed
        - builds queries as a directed acyclic graph (DAG). Has a DAG scheduler and query optimizer to break the query into tasks that can be spread across workers in the cluster
        - Has a "whole stage code generation" engine as "physical engine" - this creates compact code for execution
        - All intermediate results retained in memory so has limited disk I/O
    2. Ease of use
        - fundamental abstraction is a single logical data structure: Resilient Distributed Dataset (RDD)
        - all higher level abstractions build on RDD e.g. DataFrame, Dataset
    3. Modular
        - You can run the same Spark "application" or code from any of it's supported languages
    4. Extensible
        - Hadoop coupled storage and compute, Spark separates them
        - `DataFrameReader` and `DataFrameWriter` classes can be extended to any data source
    -   has libraries with good APIs for
        -   Machine Learning (MLib)
        -   SQL (Spark SQL)
        -   Stream processing (Structured Streaming) for real time data
        -   Graph processing (GraphX)
-   Spark has 4 components, all of which are entry points to the Spark core and Spark SQL engine
    1. SparkSQL + DataFrames + Datasets
    2. Spark Streaming (Structured Streaming)
        - Since Spark 2.0
        - built on top of Spark SQL engine and DataFrame APIs
        - lets Spark interact with Kafka, Kinesis, HDFS storage, cloud storage etc.
        - views a stream as a continuously growing table with new rows appending to the end. you can query it like a static table
    3. MLib (Machine learning)
        - contains common ML algorithms built atop DataFrame based APIs to build models
    4. GraphX (Graph processing)
        - lib for manipulating graphs e.g. social network graphs, routes, network topologies
        - performs graph-parallel computation
        - includes common algorithms e.g. PageRank, Connected Components, Triangle Counting
-   How Spark works
    1. You use the Spark APIs in any of the supported languages to write your "Spark application".
    2. Spark converts it into a DAG of java bytecode (the same bytecode is generated no matter which language you use)
        - Spark has a DSL in Python/R/Java/Scala for creating Spark applications
    3. Spark runs the DAG bytecode on the workers across the cluster

## Architecture

`SparkSession` is the (since 2.0) unified entry point for all Spark operations

-   There are 3 important concepts which are created on your local JVM when you run a Spark app (TODO: verify)
    1. Spark Application
        - contains the driver
    2. Spark Driver
        - this is the bit that talks to the cluster manager and
            - requests resources (CPU, memory) from the cluster manager for Spark's executors (JVMs)
            - transforms all the Spark operations into DAG computations
            - schedules the computations
            - distributes their execution as tasks across the executors
            - communicates directly with executors to get results
        - creates the session
    3. Spark Session
        - holds
            - create JVM runtime params
            - define DataFrames and Datasets
            - read from data sources
            - access catalog metadata
            - issue SQL queries
            - in the Spark shell, the `SparkSession` is created for you and accessible via global variable `Spark` or `sc`
-   Spark executor
    -   runs on each worker node in the cluster
    -   communicates with the driver program
    -   executes tasks on the worker
    -   usually only one executor per node
-   Cluster manager
    -   manages resources for the cluster of nodes on which a single Spark application runs
    -   currently 4 options
        1. Built-in standalone cluster manager
        2. Apache Hadoop YARN
        3. Apache Mesos
        4. Kubernetes

## Deployment modes

1. Local (single node mode)
    - cluster manager: runs on local host
    - driver: runs on single JVM
    - executor: runs in same JVM as driver
2. Standalone
    - cluster manager: any host in the cluster
    - driver: can run on any node in the cluster
    - executor: each node in cluster runs an executor JVM
3. YARN (client)
    - cluster manager: YARN resource master & YARN application master
    - driver: runs on a client now in the cluster
    - executor: YARN's NodeManager's container
4. YARN (cluster)
    - cluster manager: YARN resource master & YARN application master
    - driver: YARN master
    - executor: YARN's NodeManager's container
5. Kubernetes
    - cluster manager: kubernetes master
    - driver: runs in a k8s pod
    - executor: runs within it's own pod

## Data partitions

-   A very important data structure
-   Operations are have either wide or narrow dependencies depending on how many partitions they need as intput (1 = narrow, 1+ = wide)
-   Data is distributed across sotrage (either HDFS or cloud storage) as _partitions_
-   Each partition is abstracted as a DataFrame in memory.
-   Ideally each Spark executor is allocated a task what requires it to read the partition closest to it
-   You control how many partitions are created from your data in your Spark application via `repartition(N)`
    -   you can tune it to the number of executor cores you have available to get maximum parallelism
        ```python
        big_data_frame = Spark.read.text("path_to_large_text_file").repartition(8)
        print(big_data_frame.rdd.getNumPartitions())
        # => 8
        ```

## Spark execution: Driver, Jobs, Stages, Tasks

-   Spark driver creates many Spark jobs
-   Spark jobs
    -   during an interactive session in a Spark shell, the driver converts your Spark applicaiton into one or more "Spark jobs"
    -   each job is translated into a DAG
    -   each node wihtin the DAG is one or more "Spark stages"
-   Spark stages
    -   created based on which tasks can be performed serially or in parallel
    -   some Spark operations take more than one stage
    -   stages can be decided by the operators(?) computation boundaries
-   Spark tasks
    -   each stage is many Spark tasks
    -   a Spark task is a unit of exeicution
    -   a task matps to a single core and works on a single partition of data
-   Operations
    -   Spark operations can be classified as either _transformations_ or _actions_
        1. Tranformations
            - Immutable
            - transform a DataFrame into a new DataFrame
            - examples
                - `read()` (NB data isn't read off disk until needed)
                - `select()`
                - `filter()` (narrow deps)
                - `contains()` (narrow deps)
                - `join()`
                - `groupBy()` (wide deps)
                - `orderBy()` (wide deps)
            - evaluated lazily - they are initially recorded as a "lineage"
            -   - the lineage record allows Spark execution plan to rearrange and coalesce transformations into stages for efficient execution
            - the lineage can be replayed in case of failures making Spark resilient
            - can have either wide or narrow dependencies
                - narrow dependencies
                    - a single output partition can be computed from a single input partition
                - wide dependencies
                    - requires output from other partitions to compute its output
        2. Actions
            - an action triggers the lazy evaluation of all recorded transformations
            - examples
                - `show()`
                - `take()`
                - `count()`
                - `collect()`
                - `save()`
    -   Actions and transformations together make a _query plan_
    -   Spark query optimiser builds the stages to optimise execution of the plan
        -   join some operations
        -   pipeline some operations
        -   break operations into stages based on which operations require data to be moved across the cluster

## Running pySpark locally

pySpark is a python shell communicating with a JVM subprocess which runs all the Spark stuff

```bash
$ pySpark
# enters python shell with Spark session loaded as global `Spark`

# python 3 launches a JVM subprocess
$ pstree 62069
─┬◆ 62069 eoinkelly python3
 └─── 62144 eoinkelly /Library/Java/JavaVirtualMachines/jdk-17.0.1.jdk/Contents/Home/bin/java -cp /Users/eoinkelly/.pyen

# Full launch command of the JVM subprocess from `ps aux`
/Library/Java/JavaVirtualMachines/jdk-17.0.1.jdk/Contents/Home/bin/java
    -cp /Users/eoinkelly/.pyenv/versions/3.11.6/lib/python3.11/site-packages/pySpark/conf:/Users/eoinkelly/.pyenv/versions/3.11.6/lib/python3.11/site-packages/pySpark/jars/*
    -Xmx1g
    -XX:+IgnoreUnrecognizedVMOptions
    --add-opens=java.base/java.lang=ALL-UNNAMED
    --add-opens=java.base/java.lang.invoke=ALL-UNNAMED
    --add-opens=java.base/java.lang.reflect=ALL-UNNAMED
    --add-opens=java.base/java.io=ALL-UNNAMED
    --add-opens=java.base/java.net=ALL-UNNAMED
    --add-opens=java.base/java.nio=ALL-UNNAMED
    --add-opens=java.base/java.util=ALL-UNNAMED
    --add-opens=java.base/java.util.concurrent=ALL-UNNAMED
    --add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED
    --add-opens=java.base/jdk.internal.ref=ALL-UNNAMED
    --add-opens=java.base/sun.nio.ch=ALL-UNNAMED
    --add-opens=java.base/sun.nio.cs=ALL-UNNAMED
    --add-opens=java.base/sun.security.action=ALL-UNNAMED
    --add-opens=java.base/sun.util.calendar=ALL-UNNAMED
    --add-opens=java.security.jgss/sun.security.krb5=ALL-UNNAMED
    -Djdk.reflect.useDirectMethodHandle=false
    org.apache.Spark.deploy.SparkSubmit
    --name PySparkShell
    pySpark-shell
```

```python
# SparkSession available as 'Spark'.
>>> Spark.version
'3.5.1'
>>> strings = Spark.read.text("./package.json")
>>> type(strings)
<class 'pySpark.sql.dataframe.DataFrame'>
>>> strings.count()
15
>>> strings.show(5, truncate=False)
+----------------------+
|value                 |
+----------------------+
|{                     |
|  "name": "data-play",|
|  "version": "1.0.0", |
|  "description": "",  |
|  "main": "index.js", |
+----------------------+
only showing top 5 rows
```

Every computation expressed in the high-level structured APIs (e.g. `strings.count()` above) is:

1. Decomposed into low-level optimised and generated RDD operations
    - The generated RDD operations code
        1. is not accessible to users
        2. is not the same as the user-facing RDD APIs
2. converted into Scala **bytecode** for use by the executor(s) JVM(s)

## Spark UI

-   Spark has a web UI
-   When `pySpark` is running I also have http://localhost:4040/ web GUI available to see stats etc. - it seems read-only
-   shows jobs, stages, tasks
-   lets you monitor the performance of your jobs

## Data locality and S3

https://www.databricks.com/blog/2017/05/31/top-5-reasons-for-choosing-s3-over-hdfs.html#:~:text=superior%20to%20HDFS%27.-,Performance%20per%20Dollar,-The%20main%20problem

-   You can't optimise for data locality with S3
-   But performance per dollar is overall 2x compared to HDFS
    -   HDFS can be 6x higher read throughput than S3 (if you have perfect data locality)
    -   S3 is 10x cheaper than HDFS
    -   Storage and compute are separate so you can start bigger compute for shorter periods
    -   Databricks find that S3 is approx. 2x per per dollar

## Perf

### Scala vs Python

-   In general: Use what you want, performance is close enough to not matter in most cases
-   performance is almost identical if you use the high-level DataFrame APIs - most of the work happens in Spark anyway
-   Scala is a bit faster if you are using RDD APIs because it doesn't have the overhead of the python process communicating with the JVM
    -   That _might_ matter depending on your application

### Tuning

https://spark.apache.org/docs/latest/sql-performance-tuning.html
TODO

## pyspark

-   has a full Spark API
-   also supports the Pandas API so you can run your Pandas code on Spark easily

## Spark SQL

-   can act as a distributed query engine providing interfaces for:
    1. JDBC/ODBC
    2. Command line
-   is the basis of Dataset and DataFrame APIs

### Compression

-   Spark SQL uses compression for Parquet and Orc tables.
-   Default compression scheme is `snappy`
-   The scheme is controlled by `spark.sql.parquet.compression.codec` option
    -   If either `compression` or `parquet.compression` is specified in the table-specific options/properties, the precedence would be `compression`, `parquet.compression`, `spark.sql.parquet.compression.codec`.
    -   Acceptable values include:
        1.  none
        2.  uncompressed
        3.  snappy
        4.  gzip
        5.  lzo
        6.  brotli
            -   requires `BrotliCodec` to be installed.
        7.  lz4
        8.  zstd
            -   requires `ZStandardCodec` to be installed before Hadoop 2.9.0,

## Spark APIs

-   DataFrames and Datasets APIs are built on the Spark SQL engine

### Low level RDD (Resilient Distributed Datasets) API

-   RDD is the most basic abstraction in Spark
-   Higher level APIs are built on RDD
-   has 3 attributes
    1. Dependencies
        - a list of dependencies that tells spark how an RDD is built with it's inputs
        - When outputs are needed, Spark can take the deps and replicate the operations in case of failure
    2. Partitions (with locality information)
        - lets Spark split the work to parallelize computation across executors
        - if reading from HDFS, Spark will use locality information to send work to those executors close to the data minimizing network transfer.
    3. A compute function of type `Partition => Iterator[T]`
        - produces an `Iterator[T]` for the data that will be stored in the RDD
        - Spark doesn't see the inside of the compute function - it only sees a lambda
        - it can serialise it as an opaque series of bytes
        - it cannot optimise it, cannot re-arrange computations
-   RDD code has lambda calculus vibes - you pass in lambdas to map and reduce to get the job done.

    ```python
    dataRDD = sc.parallelize([("Brooke", 20), ("Denny", 31), ("Jules", 30),
    ("TD", 35), ("Brooke", 25)])
    # Use map and reduceByKey transformations with their lambda
    # expressions to aggregate and then compute average

    agesRDD = (dataRDD
    .map(lambda x: (x[0], (x[1], 1)))
    .reduceByKey(lambda x, y: (x[0] + y[0], x[1] + y[1]))
    .map(lambda x: (x[0], x[1][0]/x[1][1])))
    ```

-   Downsides:
    -   -- Opaque to Spark - it cannot optimise
    -   -- Hard to read for humans
    -   -- Syntax looks very different between Python/R/Scala/Java
-   The higher level APIs fix these downsides by letting us say _what_ we want and let Spark do the _how_
-   In practice, it is rare to need the RDD API

### DataFrame API

-   inspired by pandas DataFrames
-   conceptually a table
-   provides by Spark SQL
-   DataFrames **are immutable** - allows spark to keep a lineage of all transformations
-   DataFrames are distributed in-memory tables with named columns and schemas
-   each column has a data type
-   can be constructed from (https://spark.apache.org/docs/latest/sql-data-sources.html)
    -   structured data tables (CSV, JSON, Parquet, ORC, Delta Lake etc.)
    -   Arvo files
    -   Protobufs
    -   tables in Hive
    -   external databases using JDBC
    -   existing RDDs
-   If you register a DataFrame as a _temporary view_ you can run SQL queries against it

Spark basic data types in Python

| #   | Data type     | Value assigned in Python                             | API to instantiate                      |
| --- | ------------- | ---------------------------------------------------- | --------------------------------------- |
| 1.  | ByteType      | int                                                  | DataTypes.ByteType                      |
| 2.  | ShortType     | int                                                  | DataTypes.ShortType                     |
| 3.  | IntegerType   | int                                                  | DataTypes.IntegerType                   |
| 4.  | LongType      | int                                                  | DataTypes.LongType                      |
| 5.  | FloatType     | float                                                | DataTypes.FloatType                     |
| 6.  | DoubleType    | float                                                | DataTypes.DoubleType                    |
| 7.  | StringType    | str                                                  | DataTypes.StringType                    |
| 8.  | BooleanType   | bool                                                 | DataTypes.BooleanType                   |
| 9.  | DecimalType   | decimal.Decimal                                      | DecimalType                             |
| 10. | BinaryType    | bytearray                                            | BinaryType()                            |
| 11. | TimestampType | datetime.datetime                                    | TimestampType()                         |
| 12. | DateType      | datetime.date                                        | DateType()                              |
| 13. | ArrayType     | List, tuple, or array                                | ArrayType(dataType, [nullable])         |
| 14. | MapType       | dict                                                 | MapType(keyType, valueType, [nullable]) |
| 15. | StructType    | List or tuple                                        | StructType([fields])                    |
| 16. | StructField   | A value type corresponding to the type of this field | StructField(name, dataType, [nullable]) |

-   DataFrame schemas

    -   DataFrames can have optional schemas
    -   Schema defines
        1. column names
        2. data type for each column
    -   Defining schema beforehand is a best practice - benefits:
        1. Spark doesn't have to do data type inference
        2. Avoids Spark having to read a large portion of the file to figure out the schema (is expensive for large files)
        3. You get errors if the data doesn't match the schema
    -   Schemas can be defined with code or with the DDL (DDL is simpler in most cases)
    -   Example

        ```python
        # define in code
        from pyspark.sql.types import *
        schema = StructType([StructField("author", StringType(), False),
          StructField("title", StringType(), False),
          StructField("pages", IntegerType(), False)])

        # define with SQL-alike DDL
        schema = "author STRING, title STRING, pages INT"

        my_data = ...

        # using the schema
        my_df = spark.createDataFrame(my_data, schema)

        my_df.schema # getter

        print(my_df.printSchema()) # dump schema to console
        ```

-   Columns and expressions
    -   You can reference named columns to do computations
        -   `col("ColName")` returns a ref to the column which you can do computations with e.g. `col("ColName") * 2`
        -   `expr("ColName operator other")` lets you do computations with columns `expr("MyCol * 2")`
    -   Each column is part of a row.
        -   => Spark is organised by row not by column
    -   All rows together make a DataFrame (`DataFrame` is `Dataset[Row]`)
-   Rows

    -   `Row()` method builds a `Row` object
    -   You can use `Row()` to build a data frame

        ```python
        from pyspark.sql import Row
        my_row = Row(3, "Title", "body", "url", array("twitter", "linkedin"))
        my_row[1] # => "Title"

        rows = [Row("Matei Zaharia", "CA"), Row("Reynold Xin", "CA")]
        authors_df = spark.createDataFrame(rows, ["Authors", "State"])
        authors_df.show()
        # +-------------+-----+
        # |      Authors|State|
        # +-------------+-----+
        # |Matei Zaharia|   CA|
        # |  Reynold Xin|   CA|
        # +-------------+-----+
        ```

-   `DataFrameReader` and `DataFrameWriter`

    -   Spark objects which read many data sources into DataFrame and write DataFrame to many sources

        ```python
        # the DataFrameReader is held in `spark`
        >>> type(spark.read)
        <class 'pyspark.sql.readwriter.DataFrameReader'>

        # the DataFrameWriter is held on the DataFrame itself
        ```

    -   DataFrameReader
    -   DataFrameWriter
        -   default format is parquet

### Dataset API (Java/Scala only)

-   Dataset is a distributed collection of data
-   similar to RDDs but use a specialised encoder for serialising objects over network so they mitigate some downsides of RDDs
-   only available in Java/Scala
-   Python/R already have a lot of what Datasets provides for Java/Scala
-   A DataFrame is a Dataset organised into named columns
-   They type of `DataFrame` in Scala is `Dataset[Row]` - data frames are sometimes referred to as "datasets of rows"

## Appendices

### Appendix: jars in Spark distribution

```sh
# ./jars
total 323M
  134K HikariCP-2.5.1.jar
  228K JLargeArrays-1.5.jar
  1.2M JTransforms-3.1.jar
  433K RoaringBitmap-0.9.45.jar
  232K ST4-4.0.4.jar
   68K activation-1.1.1.jar
  249K aircompressor-0.26.jar
  1.2M algebra_2.12-2.0.1.jar
   19K annotations-17.0.0.jar
  164K antlr-runtime-3.5.2.jar
  329K antlr4-runtime-4.9.3.jar
   27K aopalliance-repackaged-2.6.1.jar
   75K arpack-3.0.3.jar
  1.2M arpack_combined_all-0.1.jar
  108K arrow-format-12.0.1.jar
  109K arrow-memory-core-12.0.1.jar
   39K arrow-memory-netty-12.0.1.jar
  1.8M arrow-vector-12.0.1.jar
   20K audience-annotations-0.5.0.jar
  598K avro-1.11.2.jar
  182K avro-ipc-1.11.2.jar
  185K avro-mapred-1.11.2.jar
  163K blas-3.0.3.jar
  109K bonecp-0.8.0.RELEASE.jar
   79K breeze-macros_2.12-2.1.0.jar
   13M breeze_2.12-2.1.0.jar
  3.2M cats-kernel_2.12-2.1.1.jar
   58K chill-java-0.10.0.jar
  208K chill_2.12-0.10.0.jar
   57K commons-cli-1.5.0.jar
  353K commons-codec-1.16.0.jar
  575K commons-collections-3.2.2.jar
  735K commons-collections4-4.4.jar
  170K commons-compiler-3.1.9.jar
  1.1M commons-compress-1.23.0.jar
  163K commons-crypto-1.1.0.jar
  157K commons-dbcp-1.4.jar
  473K commons-io-2.13.0.jar
  278K commons-lang-2.6.jar
  574K commons-lang3-3.12.0.jar
   61K commons-logging-1.1.3.jar
  2.2M commons-math3-3.6.1.jar
   94K commons-pool-1.5.4.jar
  233K commons-text-1.10.0.jar
   80K compress-lzf-1.1.2.jar
  2.4M curator-client-2.13.0.jar
  198K curator-framework-2.13.0.jar
  278K curator-recipes-2.13.0.jar
  359K datanucleus-api-jdo-4.2.4.jar
  2.0M datanucleus-core-4.1.17.jar
  1.9M datanucleus-rdbms-4.1.19.jar
  813K datasketches-java-3.3.0.jar
  127K datasketches-memory-2.1.0.jar
  3.1M derby-10.14.2.0.jar
   16K dropwizard-metrics-hadoop-metrics2-reporter-0.1.2.jar
   64K flatbuffers-java-1.12.0.jar
  186K gson-2.2.4.jar
  2.1M guava-14.0.1.jar
   19M hadoop-client-api-3.3.4.jar
   29M hadoop-client-runtime-3.3.4.jar
  3.3M hadoop-shaded-guava-1.1.1.jar
   56K hadoop-yarn-server-web-proxy-3.3.4.jar
  180K hive-beeline-2.3.9.jar
   44K hive-cli-2.3.9.jar
  426K hive-common-2.3.9.jar
   11M hive-exec-2.3.9-core.jar
  114K hive-jdbc-2.3.9.jar
  319K hive-llap-common-2.3.9.jar
  7.9M hive-metastore-2.3.9.jar
  896K hive-serde-2.3.9.jar
  1.7M hive-service-rpc-3.1.3.jar
   53K hive-shims-0.23-2.3.9.jar
  8.6K hive-shims-2.3.9.jar
  118K hive-shims-common-2.3.9.jar
   13K hive-shims-scheduler-2.3.9.jar
  253K hive-storage-api-2.8.1.jar
  196K hk2-api-2.6.1.jar
  199K hk2-locator-2.6.1.jar
  129K hk2-utils-2.6.1.jar
  768K httpclient-4.5.14.jar
  321K httpcore-4.4.16.jar
   27K istack-commons-runtime-3.0.8.jar
  1.4M ivy-2.5.1.jar
   74K jackson-annotations-2.15.2.jar
  537K jackson-core-2.15.2.jar
  227K jackson-core-asl-1.9.13.jar
  1.6M jackson-databind-2.15.2.jar
   54K jackson-dataformat-yaml-2.15.2.jar
  121K jackson-datatype-jsr310-2.15.2.jar
  763K jackson-mapper-asl-1.9.13.jar
  502K jackson-module-scala_2.12-2.15.2.jar
   25K jakarta.annotation-api-1.3.5.jar
   18K jakarta.inject-2.6.1.jar
   81K jakarta.servlet-api-4.0.3.jar
   90K jakarta.validation-api-2.0.2.jar
  138K jakarta.ws.rs-api-2.1.6.jar
  113K jakarta.xml.bind-api-2.3.2.jar
  937K janino-3.1.9.jar
  776K javassist-3.29.2-GA.jar
  244K javax.jdo-3.2.0-m3.jar
  386K javolution-5.5.1.jar
  990K jaxb-runtime-2.3.2.jar
   21K jcl-over-slf4j-2.0.7.jar
  197K jdo-api-3.0.1.jar
  295K jersey-client-2.40.jar
  1.2M jersey-common-2.40.jar
   32K jersey-container-servlet-2.40.jar
   72K jersey-container-servlet-core-2.40.jar
   77K jersey-hk2-2.40.jar
  929K jersey-server-2.40.jar
  263K jline-2.14.6.jar
  624K joda-time-2.12.5.jar
  418K jodd-core-3.5.2.jar
   12K jpam-1.1.jar
   25K json-1.8.jar
   88K json4s-ast_2.12-3.7.0-M11.jar
  514K json4s-core_2.12-3.7.0-M11.jar
   37K json4s-jackson_2.12-3.7.0-M11.jar
  341K json4s-scalap_2.12-3.7.0-M11.jar
   33K jsr305-3.0.0.jar
   15K jta-1.1.jar
  5.6K jul-to-slf4j-2.0.7.jar
  402K kryo-shaded-4.0.2.jar
  383K kubernetes-client-6.7.2.jar
  476K kubernetes-client-api-6.7.2.jar
   31K kubernetes-httpclient-okhttp-6.7.2.jar
  939K kubernetes-model-admissionregistration-6.7.2.jar
 1013K kubernetes-model-apiextensions-6.7.2.jar
  519K kubernetes-model-apps-6.7.2.jar
  931K kubernetes-model-autoscaling-6.7.2.jar
  290K kubernetes-model-batch-6.7.2.jar
  142K kubernetes-model-certificates-6.7.2.jar
   29K kubernetes-model-common-6.7.2.jar
   48K kubernetes-model-coordination-6.7.2.jar
  4.7M kubernetes-model-core-6.7.2.jar
  188K kubernetes-model-discovery-6.7.2.jar
  102K kubernetes-model-events-6.7.2.jar
  567K kubernetes-model-extensions-6.7.2.jar
  999K kubernetes-model-flowcontrol-6.7.2.jar
 1013K kubernetes-model-gatewayapi-6.7.2.jar
   77K kubernetes-model-metrics-6.7.2.jar
  564K kubernetes-model-networking-6.7.2.jar
  156K kubernetes-model-node-6.7.2.jar
  334K kubernetes-model-policy-6.7.2.jar
  171K kubernetes-model-rbac-6.7.2.jar
  223K kubernetes-model-resource-6.7.2.jar
   65K kubernetes-model-scheduling-6.7.2.jar
  387K kubernetes-model-storageclass-6.7.2.jar
  848K lapack-3.0.3.jar
 1022K leveldbjni-all-1.8.jar
  307K libfb303-0.9.3.jar
  241K libthrift-0.12.0.jar
  337K log4j-1.2-api-2.20.0.jar
  306K log4j-api-2.20.0.jar
  1.8M log4j-core-2.20.0.jar
   26K log4j-slf4j2-impl-2.20.0.jar
   13K logging-interceptor-3.12.12.jar
  667K lz4-java-1.8.0.jar
  7.1M mesos-1.4.3-shaded-protobuf.jar
  127K metrics-core-4.2.19.jar
   24K metrics-graphite-4.2.19.jar
   22K metrics-jmx-4.2.19.jar
   17K metrics-json-4.2.19.jar
   25K metrics-jvm-4.2.19.jar
  5.6K minlog-1.3.0.jar
  4.4K netty-all-4.1.96.Final.jar
  300K netty-buffer-4.1.96.Final.jar
  337K netty-codec-4.1.96.Final.jar
  642K netty-codec-http-4.1.96.Final.jar
  472K netty-codec-http2-4.1.96.Final.jar
  119K netty-codec-socks-4.1.96.Final.jar
  645K netty-common-4.1.96.Final.jar
  545K netty-handler-4.1.96.Final.jar
   25K netty-handler-proxy-4.1.96.Final.jar
   37K netty-resolver-4.1.96.Final.jar
  479K netty-transport-4.1.96.Final.jar
  144K netty-transport-classes-epoll-4.1.96.Final.jar
  106K netty-transport-classes-kqueue-4.1.96.Final.jar
   40K netty-transport-native-epoll-4.1.96.Final-linux-aarch_64.jar
   39K netty-transport-native-epoll-4.1.96.Final-linux-x86_64.jar
   25K netty-transport-native-kqueue-4.1.96.Final-osx-aarch_64.jar
   25K netty-transport-native-kqueue-4.1.96.Final-osx-x86_64.jar
   43K netty-transport-native-unix-common-4.1.96.Final.jar
   49K objenesis-3.3.jar
  418K okhttp-3.12.12.jar
   87K okio-1.15.0.jar
   20K opencsv-2.3.jar
  2.7M orc-core-1.9.2-shaded-protobuf.jar
  1.7M orc-mapreduce-1.9.2-shaded-protobuf.jar
   29K orc-shims-1.9.2.jar
   64K oro-2.0.8.jar
   20K osgi-resource-locator-1.0.3.jar
   34K paranamer-2.8.jar
  2.0M parquet-column-1.13.1.jar
   95K parquet-common-1.13.1.jar
  830K parquet-encoding-1.13.1.jar
  710K parquet-format-structures-1.13.1.jar
  982K parquet-hadoop-1.13.1.jar
  2.0M parquet-jackson-1.13.1.jar
   54K pickle-1.3.jar
  122K py4j-0.10.9.7.jar
   57M rocksdbjni-8.3.2.jar
  242K scala-collection-compat_2.12-2.7.0.jar
   11M scala-compiler-2.12.18.jar
  5.2M scala-library-2.12.18.jar
  183K scala-parser-combinators_2.12-2.3.0.jar
  3.6M scala-reflect-2.12.18.jar
  456K scala-xml_2.12-2.1.0.jar
  2.5K shims-0.9.45.jar
   63K slf4j-api-2.0.7.jar
  327K snakeyaml-2.0.jar
  286K snakeyaml-engine-2.6.jar
  2.0M snappy-java-1.1.10.3.jar
   12M Spark-catalyst_2.12-3.5.1.jar
  239K Spark-common-utils_2.12-3.5.1.jar
   14M Spark-core_2.12-3.5.1.jar
  423K Spark-graphx_2.12-3.5.1.jar
  558K Spark-hive-thriftserver_2.12-3.5.1.jar
  708K Spark-hive_2.12-3.5.1.jar
  529K Spark-kubernetes_2.12-3.5.1.jar
   83K Spark-kvstore_2.12-3.5.1.jar
   77K Spark-launcher_2.12-3.5.1.jar
  291K Spark-mesos_2.12-3.5.1.jar
  115K Spark-mllib-local_2.12-3.5.1.jar
  5.9M Spark-mllib_2.12-3.5.1.jar
  2.4M Spark-network-common_2.12-3.5.1.jar
  174K Spark-network-shuffle_2.12-3.5.1.jar
   38K Spark-repl_2.12-3.5.1.jar
   31K Spark-sketch_2.12-3.5.1.jar
  1.7M Spark-sql-api_2.12-3.5.1.jar
  9.2M Spark-sql_2.12-3.5.1.jar
  1.1M Spark-streaming_2.12-3.5.1.jar
   15K Spark-tags_2.12-3.5.1.jar
   54K Spark-unsafe_2.12-3.5.1.jar
  344K Spark-yarn_2.12-3.5.1.jar
  113K spire-macros_2.12-0.17.0.jar
  8.3K spire-platform_2.12-0.17.0.jar
   35K spire-util_2.12-0.17.0.jar
  7.0M spire_2.12-0.17.0.jar
   26K stax-api-1.0.1.jar
  174K stream-2.9.6.jar
   92K super-csv-2.2.0.jar
  251K threeten-extra-1.7.1.jar
  2.1M tink-1.9.0.jar
   15K transaction-api-1.1.jar
  437K univocity-parsers-2.9.1.jar
  257K xbean-asm9-shaded-4.23.jar
  114K xz-1.9.jar
   35K zjsonpatch-0.3.0.jar
  1.2M zookeeper-3.6.3.jar
  245K zookeeper-jute-3.6.3.jar
  5.7M zstd-jni-1.5.5-4.jar
```

### Delta table deep dive

A single very small delta table on S3 looks like:

```
2024-03-29 19:52:39          0 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/
2024-03-29 19:52:42          0 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/
2024-03-29 19:52:54          0 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/.s3-optimization-0
2024-03-29 19:52:54          0 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/.s3-optimization-1
2024-03-29 19:52:54          0 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/.s3-optimization-2
2024-03-29 19:53:03       2909 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/00000000000000000000.crc
2024-03-29 19:52:55       1877 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/_delta_log/00000000000000000000.json
2024-03-29 19:52:50     176966 unity-catalog/45523396836864/__unitystorage/catalogs/0de36687-3155-42e4-b136-f4a9d5c56ee9/tables/47fae4c0-92f1-425b-9275-8f0504afff7c/part-00000-9b9fa217-e434-4a1b-b1b9-f6c080f9f188.c000.snappy.parquet
```
