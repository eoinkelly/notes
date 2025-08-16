# Databricks

https://www.databricks.com/

## Comparing with AWS

- (++ ) DB much more integrated. A lot less setup complexity and dev/ops time &
  complexity
    - AWS have come at it by building a bunch of separate services and then
      loosely integrating them
    - Databricks are building a single product
    - You will need developers a lot less with Databricks
    - > For example, to achieve features similar to Databricks (specifically
      > Databricks' data engineering, business intelligence, and machine
      > learning features), I would need to host the following on AWS: IAM, EC2,
      > EMR, Glue, S3, Hive, VPCs, CloudWatch, Lambdas, and SageMaker. If there
      > is poor integration between these services, or a configuration is
      > missing, then parts of the custom-built data platform will stop working.
      > https://www.reddit.com/r/dataengineering/comments/10kjilb/comment/j5thfh4/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
- (++ AWS) if you only use a narrow subset of what Databricks does for you then
  you might be better on AWS
- (++ DB) Databricks works exactly the same on any cloud
- (++ AWS) if you want to use tools that are not bundled in DB - AWS lets you
  run almost anything.
    - ?? how much does this matter to data analysts?
- (==) because storage layer is separate, you can run other compute on it if you
  want
    - ?? is that true - can I just point some other tool at a delta-lake table
      and it will just work?

## Overview

- Self describes as a "Data Intelligence Platform"
- Built on lakehouse architecture
    - compute and storage layers are decoupled and you have access to and
      control of each layer
    - Snowflake and DB have similar architectures but Snowflake historically had
      a much tighter grip on the storage layer.
        - Since 2022 (so, recent) Snowflake now supports Iceberg (OSS competitor
          to the OSS Delta lake)

- Built on open source and open standards
- Built on spark but has proprietary extensions
    - > The Databricks Runtime includes additional optimizations and proprietary
      > features that build on and extend Apache Spark, including Photon, an
      > optimized version of Apache Spark rewritten in C++.
- Runs on all major cloud providers
- Features
    - Notebooks:
        - SQL
        - Python
        - Scala
        - R
    - IDEs
        - VSCode
        - PyCharm
        - RStudio
        - JupyterLab
    - "Unity" data catalog
    - Plays well with any other big-data tools e.g.
        - Power BI
        - Pandas
        - Tableaou
        - Spark
    - Has a marketplace where you can get addons, data sets etc.
        - Things in the marketplace communicate via delta sharing
    - Does ETL
        - can ingest from anything e.g. Salesforce, Google Analytics, without
          extra dev
        - built-in ETL using their SQL
        - Partners with FiveTran to let you import through FiveTran
            - Few clicks to connect. FiveTran will have a Databricks destination
              setup
            - Fivetran supports ingestion from heaps of places
    - Visualisations (within notebooks)
        - https://docs.databricks.com/en/notebooks/bamboolib.html
        - lets you use ipywidgets to create visualisations in notebooks
        - for more complex stuff you probably want to use Power BI
    - Dashboards
        - can create dashboards which execute stuff in HTML or IPython
        - https://docs.databricks.com/en/notebooks/dashboards.html
    - Workflows
        - has a fully managed orchestration service
        - > define, manage and monitor multitask workflows for ETL, analytics
          > and machine learning pipelines. With a wide range of supported task
          > types, deep observability capabilities and high reliability
        - Integrates with other workflow things
            - Azure data factory
            - Apache Airflow
            - FiveTran
            - dbt labs
            - arcion
            - matillion
    - Git repos
        - you can connect Github/Gitlab and let Databricks operate on the repo
          data
- Foundations - Apache Spark - Delta Lake (a file format which adds features to
  parquet) - MLFlow - Delta sharing (a way to realtime share changes between
  data stores) - Can share data sets, models, dashboards and notebooks - Can
  share across regions, clouds, platforms

## Uploading files

```
# File upload from the GUI
Maximum of 10 files and total upload size of 2GB
Requires a SQL warehouse or a cluster with Databricks Runtime 10.3 and above
Supported file formats: .csv, .tsv, .tab, .json, .jsonl, .avro, .parquet, .txt, or .xml
```

- "SQL Warehouse" is what clusters are calle din the UI
- Each one has
    - a size
    - a type
        - pro:
        - classic:
        - Scaling
        - Channle
        - Spot instance policy
        - Auto stop - when to stop the cluster after inactivity

needs to be started for you to upload data

## What is stored in S3

I see the following in S3

- `1446305435358672` is a parent of many paths - I think it's my organisation
  number maybe? it's in all the URLs of the UI as an `o` query param
- jobs data
    - SQL plans
        - seem to all be quite small files
    - SQL result somethings
        - when I come back to the UI, the old query data loads very quickly so I
          think the full query result is being cached
        - files vary in size a lot
    - notebook revisions
- catalogs
    - parquet file holding the CSV I uploaded
    - various files in `_delta_log/` presumably which extend the parquet to be a
      delta table
    - catalogs have a uuid which maps to a dir
    - tables have a uuid which maps to a dir
- temp dir
-

```
2024-03-22 10:27:03    6617174 sydney-prod/1446305435358672.jobs/ingestion/2024-03-21/21/_cf4043c1-22a9-429f-8858-864993cec174_8839571861257273/4588a201-debb-409d-8524-5c4dc30b3c7c
2024-03-22 14:22:02        973 sydney-prod/1446305435358672.jobs/notebook-revisions/2831479082561685-1711070520881--106b17e52fd65c7f6ce7118fa50bec2b5c95bb5173d37a3dcfe0611d4d7ebd40.dbc.lzf
2024-03-22 14:22:02        102 sydney-prod/1446305435358672.jobs/notebook-revisions/2831479082561685-1711070520881--1b403a1a0e1e2ad28ebaf7d4a55a1d80ca090f4a35895d2994af402cc0f1713a.json.lzf
2024-03-22 14:47:45        102 sydney-prod/1446305435358672.jobs/notebook-revisions/2831479082561685-1711072064345--1b403a1a0e1e2ad28ebaf7d4a55a1d80ca090f4a35895d2994af402cc0f1713a.json.lzf
2024-03-22 14:47:45       1026 sydney-prod/1446305435358672.jobs/notebook-revisions/2831479082561685-1711072064345--c8e86723e66089bdc164bc18791d2cac62daf9f8d4debfaad60f164de3fb098f.dbc.lzf
2024-03-22 10:30:55      12416 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T22:30:40Z_8bf33fdc-2f4f-48a1-9e69-f8105bd38166
2024-03-22 10:39:59      12616 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T22:39:46Z_c32a6d9b-31d2-480c-b0b1-2e60dab90e61
2024-03-22 10:41:41       3944 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T22:41:40Z_f6f7ae18-5ff3-4fda-b942-a21667a8a7c9
2024-03-22 10:41:53       3944 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T22:41:52Z_2cff7a42-917b-4cce-9bb1-82f4c9a4c222
2024-03-22 10:43:28       4480 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T22:43:27Z_eae59fc1-e0e9-4322-a2e8-37a71487bb21
2024-03-22 11:42:32        344 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-22T23:42:31Z_b2ec6513-e8cc-4d4c-a3aa-d1ccf9ced48d
2024-03-22 12:23:19        344 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:23:17Z_6ef28300-3a0e-4cea-948f-e6402d84edcc
2024-03-22 12:23:36        368 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:23:35Z_6b5f1f8d-5424-4e12-a962-640ea0e7a97b
2024-03-22 12:23:39        600 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:23:38Z_ac3e69b8-3299-481b-bed2-3e0750088623
2024-03-22 12:23:46       7512 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:23:45Z_274aa1cc-0916-444d-b5ae-6433b11e890b
2024-03-22 12:23:53        736 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:23:52Z_69d6f032-a1cf-4dc7-9a6e-35415200ce02
2024-03-22 12:24:11       1264 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-23T00:24:10Z_96540b33-20bd-480d-8190-1a50fde9f929
2024-03-22 11:42:37        565 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-28T22:42:36Z_61aeb4ec-5029-4fe6-a0f9-471cc8425c3f
2024-03-22 12:00:10        565 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-28T23:00:09Z_0a3cc1c4-dace-4786-9e05-a6c0c1ab3512
2024-03-22 12:03:28       2725 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-28T23:03:27Z_9b70d158-ffb2-4f24-bca2-b8276b255105
2024-03-22 12:24:44       1793 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-28T23:24:43Z_31957306-2e80-4647-aa3d-d3370544d803
2024-03-22 11:42:37        328 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-29T22:42:36Z_cf6d5e5d-ed19-423a-bf60-bf0d8ba05657
2024-03-22 12:00:10        328 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-29T23:00:08Z_2172e0c4-935c-4e80-a85b-564c26cd4c01
2024-03-22 12:03:28      21696 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-29T23:03:12Z_71161358-c359-42af-a89c-574919c6e8fe
2024-03-22 12:24:44       8040 sydney-prod/1446305435358672.jobs/sql/extended/results_2024-03-29T23:24:23Z_9128f1eb-3fa9-443a-a02d-86c963725ee6
2024-03-22 10:30:58       3205 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-30-56Z_e7547d88-a952-492c-9f7d-0a0d0ebb8b29
2024-03-22 10:40:01       3362 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-40-00Z_9825dc7c-56cc-4842-af9c-7d64f874950f
2024-03-22 10:41:29       4561 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-41-27Z_3bd3f3dd-b55d-41bf-8089-9f4b21dd5199
2024-03-22 10:41:40       4570 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-41-39Z_54a7b624-b696-41fb-9448-0fce41a2bc31
2024-03-22 10:41:42         20 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-41-41Z_7c7eaf58-b895-4dd1-85a0-ffd02d5d08d2
2024-03-22 10:41:55         20 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-41-54Z_2ca104ea-3b81-492b-9a42-b4eecf1e786a
2024-03-22 10:41:57        360 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-41-56Z_4f100a3c-5744-475c-ad41-f3f424bdea16
2024-03-22 10:43:30         20 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T21-43-29Z_a8102ee8-fa28-4231-9a67-a21b31af9d61
2024-03-22 11:42:34        271 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T22-42-32Z_4820682b-d10b-453f-a4e7-ad1f3a60efeb
2024-03-22 11:42:36        271 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T22-42-35Z_a30f57d2-2d08-4d99-87c4-0922a78cf233
2024-03-22 11:42:38        248 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T22-42-37Z_89a81d4b-dc27-43af-89aa-1f370d57a626
2024-03-22 12:00:07        351 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-00-05Z_fb81e97b-3f1b-4ea1-8601-bbbd8ff5600d
2024-03-22 12:00:10        255 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-00-09Z_b72c4da2-184d-4248-8f19-53d7ad6e2bd2
2024-03-22 12:03:30       4536 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-03-29Z_69876bb3-4392-4ccb-a8d2-a7f75ffe389a
2024-03-22 12:23:19        267 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-18Z_95596df1-260e-4c01-af53-7a99c4d89fb8
2024-03-22 12:23:19        359 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-18Z_c90818bf-e500-4750-beb9-f6cae70e9d1e
2024-03-22 12:23:21        271 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-20Z_136a229c-67fe-4dd9-bf17-4d800a7477ce
2024-03-22 12:23:36        272 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-35Z_89211c27-d37c-4353-b21f-445b38b72615
2024-03-22 12:23:38        272 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-37Z_9e31c392-07a9-4da7-9e4f-4774fd4d6418
2024-03-22 12:23:40        360 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-39Z_5b949fe2-d3f7-4314-acc4-c0676f8931f7
2024-03-22 12:23:47         20 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-46Z_1c078a66-a50c-45e4-9675-04536fb30f86
2024-03-22 12:23:53        355 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-52Z_08eca485-9563-45d6-9e88-0933360c8a47
2024-03-22 12:23:55        358 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-23-54Z_9ac75f74-412d-4816-91cf-33a0e6e67bc2
2024-03-22 12:24:13        832 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-24-11Z_e82092ae-2511-4977-85b5-06e257838481
2024-03-22 12:24:46       4537 sydney-prod/1446305435358672.jobs/sql/plans/plans_2024-04-20T23-24-45Z_f4bff1ee-497a-4054-9951-b2ef7d1e3e6f
2024-03-22 10:26:59          0 sydney-prod/1446305435358672/tmp/
2024-03-22 09:19:58          0 unity-catalog/
2024-03-22 10:41:06          0 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/
2024-03-22 10:41:10          0 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/
2024-03-22 10:41:26          0 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/.s3-optimization-0
2024-03-22 10:41:26          0 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/.s3-optimization-1
2024-03-22 10:41:26          0 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/.s3-optimization-2
2024-03-22 10:41:37       4425 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/00000000000000000000.crc
2024-03-22 10:41:27       3400 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/00000000000000000000.json
2024-03-22 10:41:56       4962 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/00000000000000000001.crc
2024-03-22 10:41:54       2677 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/_delta_log/00000000000000000001.json
2024-03-22 10:41:24     146283 unity-catalog/1446305435358672/__unitystorage/catalogs/9e24b401-93e8-433e-a2d2-356b927e2a73/tables/3a5c48aa-24b3-4630-a765-41ee30637f1b/part-00000-2467cf81-b281-423d-bc78-c8751f4d073b.c000.snappy.parquet
```

## What runs on EC2

The Databricks runtime.
https://docs.databricks.com/en/release-notes/runtime/14.3lts.html

A combination of

- Ubuntu
- Java (Zulu)
- Scala
- Python
- R
- Delta Lake
- A bunch of data focused python, java, R libraries
- Apache Spark
- Photon

## Other ways of access

All these methods below are sugar for the REST API

- Aside: Thrift
    - https://thrift.apache.org/
    - API codegen from a thrift definition file. Can codegen into most common
      languages
- Databricks CLI
    - written in go
    - https://github.com/databricks/cli
    - https://docs.databricks.com/en/dev-tools/cli/index.html
    - hits the Databricks HTTP rest API, nicer to use than curl
    - install
        ```bash
        $ brew tap databricks/tap
        $ brew install databricks
        $ databricks -v
        Databricks CLI v0.215.0
        ```
- SQL connectors for many languages: Go, Python, JS, Java
    - Python
        - https://github.com/databricks/databricks-sql-python
        - doesn't use ODBC or JDBC
        - Connects via Thrift API
        - Uses Arrow as the data exchange format
        - Conforms to https://peps.python.org/pep-0249/ (DB API spec)
    - Node
        - https://github.com/databricks/databricks-sql-nodejs
        - Fork of Hive Driver
        - Connects via Thrift API
- ODBC driver
    - https://www.databricks.com/spark/odbc-drivers-archive
- Terraform provider
- VSCode extension
- SDK for Java, Go, Python
    - https://github.com/databricks/databricks-sdk-py
    - sugar for using the HTTP API
- HTTP API
    - https://docs.databricks.com/api/workspace/introduction
    - looks quite complete

## SQL warehouses

- only allow the execution of SQL
- most users will use the warehouses defined by their admins
- compute resources that let you query and explore data
- notebooks can be attached to warehouses but only to do SQL
- you can make JDBC/ODBC connections to them e.g. this is how Tableau/PowerBI
  connects to them
- they will autostart in some scenarios

?? It seems you can run SQL queries on general compute. How?

## Other compute

- You want to run python/R/etc scripts - this is serviced by general compute

## Jobs

- has a fancy job runner
- allows non pro-devs to create ETL logic using tools they know
- job can contain multiple tasks
- can have multiple schedules and triggers
- tasks can have depend on each ohter
    - run later tasks only if previous one/all succeeded or various cases of
      failure
- tasks
    - task code can come from many sources: Notebook, python script, jar, python
      wheel, sql, dbt, spark submit among others
    - can control which cluster, what params required
    - can load libs from PyPI/Maven/CRAN/local/S3/DBFS
    - can set what notifications happen
    - can set thresholds for duration and retries

## Databricks runtime

https://docs.databricks.com/en/release-notes/runtime/index.html

- It includes
    - Ubuntu
    - Spark (and their proprietary C++ reimplementation called Photon)
    - Delta lake
    - Java & libs
    - Scala & libs
    - Python & libs
    - R & libs
    - GPU libs for GPU enabled clusters
- It integrates with databricks services e.g. notebooks, jobs, cluster
  management

Example list of what it contains:
https://docs.databricks.com/en/release-notes/runtime/14.3lts.html#system-environment

### Spark

https://docs.databricks.com/en/spark/index.html

> The Databricks Runtime includes additional optimizations and proprietary
> features that build on and extend Apache Spark, including Photon, an optimized
> version of Apache Spark rewritten in C++.

> You don’t need to configure or initialize a Spark context or Spark session, as
> these are managed for you by Databricks.

### Living with databricks

https://docs.databricks.com/en/compute/configure.html

When you add "general purpose" compute, you default to allocating at least 2
instances:

1 x Driver instance N-M Worker instances

what should Driver intance size compared to workers?

> A driver node runs the main function and executes various parallel operations
> on the worker nodes. The worker nodes read and write from and to the data
> sources

Some jobs/notebooks will require more of the driver, others will require more of
the worker(s) ?? when does the driver have a lot of heavy compute?

- Types of compute
    1. Single node
        - driver and worker run on same instance (1 core reserved for driver,
          all other cores allocated to worker tasks)
    2. Multi-node
        - 1 dedicated driver node
        - 1-N worker nodes (cannot be 0 worker nodes)
- Databricks worker nodes
    - are Spark executor nodes because databricks runs one executor per worker
    - have 2 private IP addresses
        1. for Databricks internal traffic
        2. Spark container intra-cluster communication
- DB Driver node
    - holds state for all notebooks attached to the compute
    - maintains the SparkContext
    - interprets all the commands you run from a notebook or a libon the compute
    - runs the Spark master that coordinates with the Spark executors
    - use a bigger driver instance if you are planning to `collect()` a lot of
      data from Spark workers and analyze them in the notebook
    - detach unused notebooks from the driver to free up resources

- Graviton instances - only partially supported -
  https://docs.databricks.com/en/compute/configure.html#graviton-limitations
    > [!WARN] Delta Live Tables is not supported on Graviton-enabled compute.
        Why?

Compute consists of one driver node and zero or more worker nodes. You can pick
separate cloud provider instance types for the driver and worker nodes, although
by default the driver node uses the same instance type as the worker node.
Different families of instance types fit different use cases, such as
memory-intensive or compute-intensive workloads.

You can also select a pool to use as the worker or driver node.

It takes approx ? mins to start a cluster on AWS

#### Cleaning up AWS

The ongoing cost of a cluster is

- NAT Gateway cost
- S3 storage

I suspect that the public IP of the NatG is baked into your workspace config on
databricks.com somehow - when I deleted and recreated a NG it broke access to
the cluster. All the AWS API stuff worked (starting and stopping instances etc.)
but Databricks could not communicate with the started instances

=> You probably have to leave that NatG in place for the lifetime of your
cluster. I could maybe delete it and keep the EIP but I haven't tested this
idea.

#### Inside the Databricks driver VM

You can get a web terminal from the Databricks UI as a tab on the details of the
compute it is a driver for

```sh
/Volumes
/dbfs
/Workspace
/databricks
    # databricks stuff lives here

root@0329-064549-55dtb45c-10-20-172-165:/databricks# pstree -a
systemd
  ├─agetty -o -p -- \\u --noclear --keep-baud console 115200,38400,9600 vt220
  ├─bash /local_disk0/tmp/_startR.sh4765161771875777701resource.r /local_disk0/tmp/_rServeScript.r4839154249780921139resource.r 1100 None
  │   └─R --slave --no-restore -f /local_disk0/tmp/_rServeScript.r4839154249780921139resource.r --args 1100
  ├─cron -f -P
  ├─dbus-daemon --system --address=systemd: --nofork --nopidfile --systemd-activation --syslog-only
  ├─goofys-dbr -f -o allow_other -o bg --file-mode=0777 --dir-mode=0777 --type-cache-ttl 0 --stat-cache-ttl 1s --http-timeout 120s --uid 65534 --gid 65534 /: /dbfs
  │   └─9*[{goofys-dbr}]
  ├─java -Dlog4j2.formatMsgNoLookups=true -XX:-UseContainerSupport -XX:+PrintFlagsFinal -XX:+PrintGCDetails -XX:+PrintGCDateStamps -verbose:gc -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=logs -XX:-OmitStackTraceInFastThrow -Xms1024m -Xmx1024m -Ddatabricks.serviceName=chauffeur-1 -cp ...
  │   ├─bash /databricks/spark/scripts/start_driver.sh /tmp/driver-env.sh
  │   │   └─java -Djava.io.tmpdir=/local_disk0/tmp -XX:-OmitStackTraceInFastThrow -Djava.security.properties=/databricks/spark/dbconf/java/extra.security -XX:-UseContainerSupport -XX:+PrintFlagsFinal -XX:+PrintGCDateStamps -XX:+PrintGCDetails -verbose:gc -Xss4m-Djava.library.path=/usr/java/packages/lib:/usr/lib64:/lib64:/lib:/usr/lib:
  │   │       ├─bash /local_disk0/tmp/_startR.sh6380735048494537006resource.r /local_disk0/tmp/_rServeScript.r5171523619771688855resource.r 1101 None
  │   │       │   └─R --slave --no-restore -f /local_disk0/tmp/_rServeScript.r5171523619771688855resource.r --args 1101
  │   │       │      └─R --slave --no-restore -f /local_disk0/tmp/_rServeScript.r5171523619771688855resource.r --args 1101
  │   │       └─185*[{java}]
  │   └─66*[{java}]
  ├─java -cp /databricks/spark/conf/:/databricks/spark/assembly/target/scala-2.12/jars/*:/databricks/spark/dbconf/log4j/master-worker/:/databricks/jars/* -Dlog4j2.formatMsgNoLookups=true -XX:+IgnoreUnrecognizedVMOptions --add-opens=java.base/java.lang=ALL-UNNAMED --add-opens=java.base/java.lang.invoke=ALL-UNNAMED--add-opens=ja
  │   └─32*[{java}]
  ├─monit -c /etc/monit/monitrc
  │   ├─(check_dbfs_fuse)
  │   └─{monit}
  ├─networkd-dispat /usr/bin/networkd-dispatcher --run-startup-triggers
  ├─rsyslogd -n -iNONE
  │   └─2*[{rsyslogd}]
  ├─sshd
  ├─start_ucfuse.sh /databricks/spark/scripts/fuse/start_ucfuse.sh
  │   └─goofys-dbr -f -o allow_other --unity_catalog --file-mode=0777 --dir-mode=0777 --type-cache-ttl 0 --stat-cache-ttl 1s --http-timeout 120s --uid 65534 --gid 65534 /: /Volumes
  │       └─9*[{goofys-dbr}]
  ├─start_wsfs.sh /databricks/spark/scripts/fuse/start_wsfs.sh
  │   └─wsfs /Workspace
  │       └─10*[{wsfs}]
  ├─systemd-journal
  ├─systemd-logind
  ├─systemd-resolve
  ├─ttyd -p 7681 -d 7 -P 30 -t disableReconnect=true -t disableLeaveAlert=true -f /tmp/wsfsttyd bash /databricks/spark/scripts/ttyd/set_wsfs_credentials.sh
  │   └─bash --rcfile /databricks/spark/scripts/ttyd/webTerminalBashrc
  │       └─pstree -a
  └─unattended-upgr /usr/share/unattended-upgrades/unattended-upgrade-shutdown --wait-for-signal
      └─{unattended-upgr}
```

#### Git integration

- When you add a repo to your Databricks workspace, it is cloned into
  `sydney-prod/45523396836864.jobs/workspace-files/` in my S3 bucket
    - The filenames in S3 are uuids - it does not use the filenames from your
      git repo
- You have to explicitly create a git commit and push any changes you make to
  the repo in Databricks
- You have to explicitly pull changes in - they don't get pulled in
  automatically
    - maybe this could be automated via Databricks API from a Github action
- Databricks is an ok web based code editor - it can edit files other than
  notebooks

#### VSCode integration

- You can run scripts from VSCode in your cluster

?? what do they run as? a new job? existing job? something else?

#### Running SQL

You cannot use all-purpose compute to run SQL ?

## Hive metastore

- each workspace has a built-in hive metastore which is (confusingly)
  represented as a catalog in the Unity Catalog
- Databricks consider this legacy now and want you to use their Unity Catalog
  metastore
- not managed by Unity Catalog
- The hive metastore is not available until you have running SQL warehouse
  compute
-

## Unity catalog

- governance for both data and AI on Databricks
- your user management and metastore are not per workspace anymore - they are
  shared across workspaces
- central place to
    - administer your data and AI models
    - audit access
- lineage of queries is captured in the unity cagalog
- Unity Catalog metastores
    - can be stored in your cloud account
    - ??? but don't have to be?

https://docs.databricks.com/en/data-governance/unity-catalog/index.html#the-unity-catalog-object-model

```
# hierarchy in the Catalog
Metastore
    Catalog
        Catalog
            Schema
                Table
                View
                Volume
                Model (not data but managed at this level like data)
                Function
```

Default Catalogs

1. <the one you create when doing setup>
2. `hive_metastore`
3. `samples`
4. `system`

- Metastores
    - registers metadata about data and AI assets
    - registers permissions for the assets
    - Best practice: Create one metastore for each region you operate in. Assign
      that metastore to all workspaces in that regions
- Catalogs
    - first layer of the Unity Catalog 3 layer `catalog.schema.*` namespace
- Schemas
    - Akin to databases
    - second layer of the Unity Catalog 3 layer `catalog.schema.*` namespace
- Tables
    1. Managed tables
        - default way to create tables
        - file layout and lifecycle fully managed by Unity Catalog
        - do not manage these table directly
        - always use the Delta table format
    2. External tables
        - data lifecycle and file layout manged outside Unity Catalog
        - use if you require direct access by tools outside Databricks
        - DROPing the table does not delete the underlying data
        - supported file formats
            - Delta
            - CSV
            - JSON
            - ARVO
            - PARQUET
            - ORC
            - TEXT
- Views
    - read-only views across multipel tables and other views
- Volumnes
    - siblings to tables and views
    - contain directoryies and files in any format
    - provide non-tabular access tod ata
    - files in volumes cannot be registered as tables
    - types
        1. Managed
            - files stored in the default location for the Unity Catalog
        2. External
            - files stored elsewhere
- Models
    - ML model registered in the _MLFlow Model Registry_
