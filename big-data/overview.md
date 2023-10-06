# Big data overview

## Sources

-   [Deciphering data architectures book](https://learning.oreilly.com/library/view/deciphering-data-architectures/9781098150754/)

## Data architectures

No standard definitions of data architectures and concepts

![Data architectures](./data-architectures-compared.png)

1. Relational data warehouse (1984)
    - storage and compute
    - schema-on-write which makes data ingestion hard
    - can be hard to scale
    - still viable solution now despite the hype around other options
    - despite the name it can use some non-relational but still structured storage e.g. columunar
1. Data lake (2010)
    - Glorified file system
    - Storage only, no compute **required** (but you still have it to do anything with the data)
    - Started with Apache Hadoop
    - schema-on-read
        - schema can be in a separate file
    - defining characteristic is data stored in it's natural/raw format (not transformed on the way in)
    - started as solutions to the problems of data warehouses
    - querying is actually pretty hard with a data lake
        - data lake vendors went out of business
    - the concept has morphed into a place to stage and prepare data
    - suggested zones within your lake
        - raw layer/landing area
        - conformed/base/standardised layer (CSV, JSON, Parquet)
        - cleansed/refined/enriched data layer
        - presentation/application/curated/consumption/gold/production-ready data layer
        - sandbox/exploration/development layer for data scientists to play in
1. Modern Data warehouse (2011)
    - use both a data warehouse and a data lake for the bits they are good at
    - ![Modern data warehouse](./modern-data-warehouse.png)
1. Data fabric (2016)
    - an evolution of MDW
    - ![Data fabric](data-fabric.png)
1. Data Lakehouse (2020)
    - portmanteau of data lake and data warehouse
    - started with Databricks
    - store your data as in a data-lake but have a layer of transactional storage software which runs on top and lets you do queries
    - can think of it as "data lake with the problems fixed"
    - Open source options
        - Apache iceberg
        - Delta lake
        - Apache Hudi
    - ![Data lakehouse](data-lakehouse.png)
1. Data mesh (2019)
    - Fashionable and hyped
    - Only suitable for narrow set of use-cases
    - decentralised
    - data stored in multiple "domains" each owned by a separate area of the business
    - data mesh is a concept not a technology - you cannot buy data mesh
    - requires an org culture shift
    - ![Data mesh](data-mesh.png)

## Categorise "big" data

Define data by 6 metrics:

1. Size/volume
    - how much data is stored
2. Speed/velocity
    - how fast is new data generated
3. Type/Variety
    - structured (RDBMS)
    - semi-structured (logs, CSV, XML, JSON)
    - unstructured (email, docs, PDFs
    - binary (audio, video)
4. Veracity
    - how accurate and reliable
5. Variability
    - how consistent is the format, the quality, the meaning
6. Value
    - how useful and relevant

## Data formats

### Delta lake

-   Important in the data lakehouse architecture
-   Docs https://docs.delta.io/latest/index.html
-   Uses versioned parquet files to store data
-   sits on top of Apache spark
-   AWS Glue 3.0 and later supports the Linux Foundation Delta Lake framework.

> Delta Lake is an open data format incubated and maintained by Databricks, the company started by the founders of Apache Spark

> Delta Lake is an open storage format used to store tabular data in data lake storage systems, offering ACID guarantees
