# Athena

> Amazon Athena uses Presto with ANSI SQL support and works with a variety of
> standard data formats, including CSV, JSON, ORC, Avro, and Parque



 Supported formats:

* CSV
* JSON
* ORC
* Avro
* Parque


large joins, window functions, and arrays

> use AWS Glue to automatically crawl data sources to discover data and populate your Data Catalog with new and modified table and partition definitions.


Presto

* https://github.com/prestodb/presto
* Runs on Hadoop
* Written in Java
* Originally by Facebook
* Open source distributed SQL query engine

> You can also connect to Athena from a wide variety of BI tools using Athena's JDBC driver.

> Athena enables you to run SQL queries across data stored in relational, non-relational, object, and custom data sources

> store results in Amazon S3 for subsequent use.

> Athena executes federated queries using Athena Data Source Connectors that run on AWS Lambda. AWS has open source data source connectors for Amazon DynamoDB, Apache HBase, Amazon Document DB, Amazon Redshift, AWS CloudWatch, AWS CloudWatch Metrics, and JDBC-compliant relational databases such MySQL, and PostgreSQL. You can use these connectors to run federated SQL queries in Athena. Additionally, using the Athena Query Federation SDK, you can build connectors to any data source.

# Glue

* Fully managed ETL service
* Runs serverless
* Jobs can be run on a schedule or based on events
* Can pull data from S3, RDS, MongoDB, basically most things
* Generates Python/Scala to do the actual transformation of your data from sources to destinations
* Can import from AWS or JDBC-compatible source
* Glue discovers your data and stores the associated metadata (e.g. table definition and schema) in the AWS Glue Data Catalog.
* Once cataloged, your data is immediately searchable, queryable, and available for ETL.



