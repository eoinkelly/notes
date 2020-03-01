# vi: ft=bash

# Cluster settings
GET /_cluster/settings

# Cluster health
GET /_cluster/health

# ########
# Cat APIs
# ########

# Add `v` query param to get headings in plain text output

# Get help on the query params you can pass to each command by adding the `help` query param
# https://www.elastic.co/guide/en/elasticsearch/reference/current/cat-indices.html
# List indices (straight from Lucene, doc count includes hidden nested docs, use _count to get a better doc count)
GET /_cat/indices?v
GET /_cat/indices?format=json
GET /_cat/indices?format=yaml
GET /_cat/indices?format=smile
GET /_cat/indices?format=cbor

# byte sizes in mb, verbose (headers) enabled, custom sorting
GET /_cat/indices?bytes=mb&s=store.size:desc&v

GET _cat/aliases?v
GET _cat/count?v
GET _cat/allocation?v
GET _cat/health?v
GET _cat/master?v
GET _cat/nodeattrs?v
GET _cat/nodes?v
GET _cat/thread_pool?v
GET _cat/pending_tasks?v
GET _cat/plugins?v
GET _cat/recovery?v
GET _cat/segments?v
GET _cat/shards?v



# ???
GET _cache/clear

# List nodes
GET /_cat/nodes?v

# All indices together
GET /_count
GET /_mapping
GET /_settings
GET /_search

# Per-index
GET catalogue-development-contributor/_count
GET catalogue-development-contributor/_mapping
GET catalogue-development-contributor/_settings
GET catalogue-development-contributor/_search

# Turn on debug logging for index
PUT /catalogue-development-contributor/_settings
{
  "index.indexing.slowlog.threshold.index.debug": "0s",
  "index.search.slowlog.threshold.fetch.debug": "0s",
  "index.search.slowlog.threshold.query.debug": "0s"
}

# test default analyzer for index
POST /catalogue-development-contributor/_analyze
{"text":"hix! ;mex"}

# test analyzer on specific field in index
POST /catalogue-development-contributor/_analyze
{
  "field": "author_note",
  "text": "example text"
}

GET catalogue-development-work/_count
GET catalogue-development-work/_mapping
GET catalogue-development-work/_settings
GET catalogue-development-work/_search
# Turn on debug logging for index
PUT /catalogue-development-work/_settings
{
  "index.indexing.slowlog.threshold.index.debug": "0s",
  "index.search.slowlog.threshold.fetch.debug": "0s",
  "index.search.slowlog.threshold.query.debug": "0s"
}

# test default analyzer for index
POST /catalogue-development-work/_analyze
{"text":"hix! ;mex"}

# test analyzer on specific field in index
POST /catalogue-development-work/_analyze
{
  "field": "author_note",
  "text": "example text"
}

GET catalogue-development-resource/_count
GET catalogue-development-resource/_mapping
GET catalogue-development-resource/_settings
GET catalogue-development-resource/_search
# Turn on debug logging for index
PUT /catalogue-development-resource/_settings
{
  "index.indexing.slowlog.threshold.index.debug": "0s",
  "index.search.slowlog.threshold.fetch.debug": "0s",
  "index.search.slowlog.threshold.query.debug": "0s"
}

# test default analyzer for index
POST /catalogue-development-resource/_analyze
{"text":"hix! ;mex"}

# test analyzer on specific field in index
POST /catalogue-development-resource/_analyze
{
  "field": "author_note",
  "text": "example text"
}


GET catalogue-development-organisation/_count
GET catalogue-development-organisation/_mapping
GET catalogue-development-organisation/_settings
GET catalogue-development-organisation/_search
# Turn on debug logging for index
PUT /catalogue-development-organisation/_settings
{
  "index.indexing.slowlog.threshold.index.debug": "0s",
  "index.search.slowlog.threshold.fetch.debug": "0s",
  "index.search.slowlog.threshold.query.debug": "0s"
}

# test default analyzer for index
POST /catalogue-development-organisation/_analyze
{"text":"hix! ;mex"}

# test analyzer on specific field in index
POST /catalogue-development-organisation/_analyze
{
  "field": "author_note",
  "text": "example text"
}
