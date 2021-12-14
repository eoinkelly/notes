
Questions:

primary key = partition key + optional sort key


## Querying

> You must provide the name of the partition key attribute and a single value
> for that attribute. Query returns all items with that partition key value.
> Optionally, you can provide a sort key attribute and use a comparison operator
> to refine the search results.

SO query takes a partition key and returns all records matching
if you provide a sort key, it is applied as a filter expression to the matched records (you still read all the records matching partition key)

    # pseudocode
    query(part_key, sort_key=nil) => all records matching part_key and sort_key constraint

> Query results are always sorted by the sort key value. If the data type of the
> sort key is Number, the results are returned in numeric order; otherwise, the
> results are returned in order of UTF-8 bytes. By default, the sort order is
> ascending. To reverse the order, set the ScanIndexForward parameter to false.

> You can query a table, a local secondary index, or a global secondary index.

If the value is not in your partition key, sort key, local secondary index or global secondary index then you can't search for it!

Conceptually query is much closer to a hash lookup than anything SQL ish

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


==============
create-table vs create-global-table

       Creates a global table from an existing table. A global table creates a
       replication relationship between two or more DynamoDB tables  with  the
       same table name in the provided Regions.

## creating a table

* table names must be unique in each region of your account
## all cli commands

```bash
aws dynamodb

batch-execute-statement o batch-get-item
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