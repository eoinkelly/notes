## Diagnosing issues

```js
// recommended settings to get better logs from slower queries
PUT / index / _settings;
{
    "index.search.slowlog.threshold.query.warn: 1s",
        "index.search.slowlog.threshold.query.info: 500ms",
        "index.search.slowlog.threshold.query.debug: 1500ms",
        "index.search.slowlog.threshold.query.trace: 300ms",
        "index.search.slowlog.threshold.fetch.warn: 500ms",
        "index.search.slowlog.threshold.fetch.info: 400ms",
        "index.search.slowlog.threshold.fetch.debug: 300ms",
        "index.search.slowlog.threshold.fetch.trace: 200ms";
}
```

## JSON API Overview

-   Responses are unformatted JSON unless you pass `?pretty=true`
    -   Kibana formats all responses by default

## Cat APIs

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/cat.html

-   Intended for humans to consume
-   Returns tabular text not JSON
-   All cat APIs are under `GET /_cat/...`
-   Handy query params
    -   `v` => show headings in tables
    -   `help` => show help output describing each column instead of actual output
-   you can control which columns are returned by query param
-   some options to control the presentation of numeric and time columns
-   can sort by different columns
-   output available in the following formats
    -   text (default)
    -   JSON `format=json` (add `pretty` to get pretty output via CURL, Kibana is always pretty)
    -   YAML `format=yaml`
    -   cbor `format=cbor`
        -   Concise Binary Object Representation
        -   a binary format loosely based on JSON
        -   https://en.wikipedia.org/wiki/CBOR
    -   smile `format=smile`
        -   a binary encoding of JSON
        -   called smile because the data header includes `:)`
        -   https://en.wikipedia.org/wiki/Smile_(data_interchange_format)

### Logging

-   Default log level is INFO
-   Changing log level to DEBUG is **very** noisy
-   Instead, turn on logging of each index, fetch and query operation
-   The logs appear in docker-compose output as you would hope.

```js
GET /_all/_settings

// * Enable detailed logging for **all** indexes.
// * You can also do this per-index.
// * Be aware that doing it for all indices makes kibana indexes very noisy
PUT /_all/_settings
{"index.indexing.slowlog.threshold.index.debug": "0s",
"index.search.slowlog.threshold.fetch.debug" : "0s",
"index.search.slowlog.threshold.query.debug": "0s"}
```
# Debugging Elasticsearch

* If you are using the searchkick gem then then Rails server log is a better place to get query logs because:
  * You don't have to mess with the ES index settings
  * The ES server slow query log is explicit about params which are defaults so is noisier which may not be helpful

## Enable slow query logging on an index

```json
# turn on slow query logging
PUT /<index-name>/_settings
{
  "index.search.slowlog.threshold.query.trace": "0s",
  "index.search.slowlog.level": "trace",
  "index.indexing.slowlog.threshold.index.trace": "0s",
  "index.indexing.slowlog.level": "trace",
  "index.indexing.slowlog.source": true
}

# reset index slow query logging to default (null values reset to default)
PUT /<index-name/_settings
{
  "index.search.slowlog.threshold.query.trace": null,
  "index.search.slowlog.level": null,
  "index.indexing.slowlog.threshold.index.trace": null,
  "index.indexing.slowlog.level": null,
  "index.indexing.slowlog.source": null
}
```

* `"index.indexing.slowlog.source": true` includes the whole source in the log (avoids truncating it). Set this to a number to set a max length.
* You don't need the `index.indexing.slowlog` stuff unless you also want slow logs from indexing operations

### Working with slow query log output

The slow query log output is hard to read so pipe it through the [./slow_query_log_filter.rb](./slow_query_log_filter.rb) script.

This query

```json
GET /page_events_development/_search
{
    "query": {
        "match": {
          "name": "approve"
        }
    }
}
```

Generates this output:

```json
{"type": "index_search_slowlog", "timestamp": "2022-11-23T00:30:40,692Z", "level": "TRACE", "component": "i.s.s.query", "cluster.name": "docker-cluster", "node.name": "e05ebce448c8", "message": "[page_events_development_20221122135217245][0]", "took": "1.3ms", "took_millis": "1", "total_hits": "9861 hits", "stats": "[]", "search_type": "QUERY_THEN_FETCH", "total_shards": "1", "source": "{\"query\":{\"match\":{\"name\":{\"query\":\"approve\",\"operator\":\"OR\",\"prefix_length\":0,\"max_expansions\":50,\"fuzzy_transpositions\":true,\"lenient\":false,\"zero_terms_query\":\"NONE\",\"auto_generate_synonyms_phrase_query\":true,\"boost\":1.0}}}}", "cluster.uuid": "i4haZ-SBQyOhC2U1MEKGPg", "node.id": "wNkGjX__TiulNRUuiYtNIA"  }
```

which is then filtered into this:

```jsonc
// XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
// timestamp: 2022-11-23T00:30:40,692Z
// took: 1.3ms
// total_hits: 9861 hits
// search_type: QUERY_THEN_FETCH
// index searched: [page_events_development_20221122135217245][0]
// source:
{
  "query": {
    "match": {
      "name": {
        "query": "approve",
        "operator": "OR",
        "prefix_length": 0,
        "max_expansions": 50,
        "fuzzy_transpositions": true,
        "lenient": false,
        "zero_terms_query": "NONE",
        "auto_generate_synonyms_phrase_query": true
      }
    }
  }
}

```