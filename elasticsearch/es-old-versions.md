# Appendix: Elasticsearch 6.x and older

> _Info_ These are some old notes which only apply if you are trying to upgrade
> a very old ES

- There were some significant changes in ES 6.x
    - The concept of mapping types was removed
        - https://www.elastic.co/guide/en/elasticsearch/reference/6.8/removal-of-types.html
        - Summary: it was inefficient at the Lucene layer to have different
          types of document in the same index
        - 6.x deprecated them
        - Before 6.x you could have multiple types per index. Now the enforce
          one type per index.
        - After 6.x the place in the APIs where you could specify a custom type
          is filled by `_doc` which roughly means "the one type in this index"
- Breaking changes in 7.x
    - The APIs were changed to no longer have space for the type name

Pre 6.x:

| Relational DB | Elasticsearch |
| ------------- | ------------- |
| Database      | Index         |
| Table         | Type          |
| Row           | Document      |
| Column        | Field         |

After 6.x:

| Relational DB | Elasticsearch               |
| ------------- | --------------------------- |
| Database      | The whole cluster (I guess) |
| Table         | Index                       |
| Row           | Document                    |
| Column        | Field                       |

You can implement your own "type" field if you really do need to store multiple
types of doc in same field because

> there is a limit to how many primary shards can exist in a cluster so you may
> not want to waste an entire shard for a collection of only a few thousand
> documents.

it seems in the examples they use `_doc` for the type e.g. `users/_doc` and
`products/_doc` \_doc is the "default type of an index in 6+

Create an index with a named mapping. This syntax only works in ES6 and older.
ES7 only allows one type per index and the default type name is `_doc`

```js
// ES6 and earlier
// create an index (eoin_test_2), a type (eoins_type) with two properties (f1, f2)
PUT /eoin_test_2
{
  "settings": {
    "index": {
      "number_of_shards": 5,
      "number_of_replicas": 2
    }
  },
  "mappings": {
    "eoins_type": {
      "properties": {
        "f1": {
          "type": "text"
        },
        "f2": {
          "type": "keyword"
        }
      }
    }
  }
}
// =>
{
  "acknowledged": true,
  "shards_acknowledged": true,
  "index": "eoin_test_2"
}
```
