# Graph databases

## Basics

- Relationships are first-class citizens in graph DBs
- Terminology
    - nodes (or vertices)
        - store data entities
    - edges
        - store relationships
        - an edge is (conceptually at least) a tuple of
          `(start-node, end-node, type, direction)` - note the **type** and
          **direction**
- Advantages
    - Queries are apparently faster for highly interconnected datasets
    - Modelling highly interconnected datasets is much more intuitive
      (allegedly, I have not verified this)

Graph DBs are "schema on read" i.e. your code needs to know the schema to read
data

## heuristics to know when to reach for a graphDB instead of a relational DB

It seems like you should start by modelling relationally and then move to graphs
if:

1. You have frequent many-many relationships which make querying the data
   difficult in a relational model
1. Your relational queries are slow and there aren't other things you can do to
   speed them up

## Popular graph databases

- Neo4j https://neo4j.com/
    - Supports Cypher and Gremlin query languages
    - Apparently this is the most popular GraphDB according to Neo4j
- Datomic https://www.datomic.com/
    - Uses _Datalog_ as a query language
    - Is an immutable append-only data storage
- Oracle supports graph data
- MS SQL Server supports graph data
- AWS Neptune
    - ACID compliant
    - Uses either _Property Graph_ or _RDF_ data models
    - Allows import of CSV data from S3
    - Allows querying in either _Gremlin_ or _SPARQL_ query languages
    - Can import from CSV, RDF, GraphML files or from a DynamoDB stream
- Azure Cosmos DB supports graph data
- Apache Tinkerpop
    - http://tinkerpop.apache.org/

### Graph models

1. Property Graph
    - Uses _Apache tinkerpop gremlin_ as query language
1. RDF
    - Uses _SPARQL_ as query language

Others: https://en.wikipedia.org/wiki/Graph_database#List_of_graph_databases

### Query languages

1. Graph DB query languages are not standardised like SQL is but there are some
   "multi-vendor" query languages:
    - SPARQL
        - works with RDF databases
    - Cypher
        - Created for Neo4j, used by Neo4j
    - Gremlin
        - Works with many backends
        - Part of the _Apache Tinkerpop_ project
1. GraphQL
    - A query language for any backend service
1. Datalog
    - Used by datomic

Graph DBs may also be accessed directly via APIs (i.e. not using a query
language)

### Underlying storage options

- Table (relational model) storage
- Key value (or document) storage
