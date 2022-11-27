# Course: Elasticsearch 8 in depth and hands on

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

## _seq_no and _primary_term

```json
{
  "_index": "movies",
  "_type": "_doc",
  "_id": "111",
  "_version": 1,
  "_seq_no": 12,
  "_primary_term": 1,
  "found": true,
  "_source": {
    "title": "Eoin 1",
    "year": 2022,
    "genre": [
      "aaa",
      "bbb"
    ]
  }
}
```

# Optimistic concurrency control

Each document returned has:

1. `_seq_no`
2. `_primary_term`
  * id of the primary shard which owns the sequence

The tuple: `(_seq_no, _primary_term)` identifies a specific revision of a document
and is used to implement optimistic concurrency control.

If you want to update only the document you got then you pass the `_seq_no` and `_primary_term` as conditions to the update - if ES finds that the most recent revision does not match your given (seq_no, primary_term) then it fails the write.

```jsonc
// This uses the (seq_no, primary_term) tuple to only write to the revision of
// the doc that we saw
POST /movies/_update/111?if_seq_no=13&if_primary_term=1
{
  "doc": {
    "title": "Eoin 2"
  }
}
```

Set `retry_on_conflict=N` param on your update request to have ES automatically retry a write if it fails due to a sequence number conflict.

```jsonc
POST /movies/_update/111?retry_on_conflict=3
{
  "doc": {
    "title": "Eoin 2"
  }
}
```