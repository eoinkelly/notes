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