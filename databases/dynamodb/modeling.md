# Modeling

Dynammo rquires more design up front than a SQL DB.

You need to know your access patterns at design time unlike with a SQL DB

Workbench facets

* a facet is a way of looking at just a single type of object within a single table design
    * they aren't called "views" because that overlaps with the meaning in a SQL DB

you can use one table per model
-- less optimized than the denormalised
?? you have to do the joins in application memory
    -- more network requests than with a SQL DB
    ?? but is it worse than say postgres?
++ might be better i fyou don't yet understand your traffic patterns

single table design
-- harder to evolve the design as rquirements change


Keep data attributes and indexing attributes separater - be ok with seeming duplication between data attributes and indexing attributes
do keep the Dynamo specfic stuff confined to classes at the edge of your system

remember that you can query on indexed attributes but not data attributes
you can scan for anything but it's very slow

?? is data mmapper as it appears in phoenix the best way here?


> “For each global secondary index you use, give it a generic name of
> GSI<Number>. Then, use GSI<Number>PK and GSI<Number>SK for your attribute types”

Add a `Type` field to your data attributes to help map back to a model in your code
    * lets you filter your scans (mmigrations)

> “DynamoDB is not great at performing ad-hoc OLAP-style queries, so you will probably import your data to Amazon Redshift or export it to Amazon S3 to query with Amazon Athena”

## Migrations

How do they work?

* a background task which scans table and changes old records to new


## ORM

Book recommends against. But maybe it's ok for usages where you don't need mega scale

## Migrations

What is a "migration" in Dynamo?

1. Scan the table, editing every row which needs editing (only the rows which correspond to the model you are migrating will need editing)
1. Create any new GSIs as required
1. Destroy any GSIs no longer required

Unless your code is able to work in the before, middle and after states of the migration then you will need downtime to achieve this