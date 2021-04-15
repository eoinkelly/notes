# Getting started with a new Kibana instance

When faced with a new ES instance (via Kibana) and you want to figure out what is going on.

## Dev tools commands

1. Go to the dev tools section
2. Paste the following into the Console and run lines as required

```bash
# Cmd-enter to run query when cursor is on same line
# add `v` query param to see table headings
# add 'help' query param to see description of table headings

# get help about what the output columns mean
GET /_cat/indices?help

# List indices (similar to \l in psql)
GET /_cat/indices?bytes=mb&s=store.size:desc&v
# Columns:
#
#   health = red|yellow|green
#   status = open| ???
#   index = name of index
#   uuid = uuid of index
#   pri = number of primary shards
#   rep = number of replica shards
#   docs.count = number of available docs
#   docs.deleted = number of deleted docs
#   store.size = store size of primaries and replicas
#   pri.store.size = store size of primaries
#
# Example:
#
#       health status index             uuid                   pri rep docs.count docs.deleted store.size pri.store.size
#       yellow open   awswaf-2021-02-17 6XGGo8fHRsSIF9CTV_t7JA   1   1       2965            0          5              5
#       yellow open   awswaf-2021-02-16 0WVipiG4TBKDDNCps6dvzw   5   1       1318            0          2              2
#       yellow open   awswaf-2021-02-18 2EqDblofS-KSF0lxC9UOjg   1   1       2261            0          2              2
#       yellow open   awswaf-2021-02-19 qpZy4fC-Q4iYBzJmxXTKCA   1   1       1388            0          2              2
#       green  open   .kibana_1         gMynp5xTQB6QfmOrw7pxMA   1   0         20            2          0              0

# When you have an index(or indices) you are interested in, run:

# search all indices (includes the kibana index so usually not what you want)
GET /_search?size=10

# search all indices which begin with 'awswaf-'
GET /awswaf-*/_search?size=5

# return the first document from a single index
GET awswaf-2021-02-17/_search?size=1
# Now you have an example of a document

# Show the mapping for an index
GET awswaf-2021-02-17/_mapping
# Now you have the mapping

# Suggestion: save the example doc and mapping into text files and compare to understand the data

# How many documents and shards in index?
GET awswaf-2021-02-17/_count

# Show the settings for the index
GET awswaf-2021-02-17/_settings

# Get document count for entire cluster
GET _cat/count?v
# Example:
#       epoch      timestamp count
#       1613762202 19:16:42  7956
```

## Kibana saved index patterns

Not sure
they seem to be what lets kibana interpret an index
how diff from ES mapping?

## Kibana saved objects

Types

1. Visualisation object
    * Appear under the "Visualise" tab in Kibana
2. Dashboard
    * Appear under the "Dashboard" tab in Kibana
3. Index pattern

They are presented as multiple JSON blobs in the "Saved Objects" UI


## Other useful commands
```bash

# Cluster settings
GET /_cluster/settings

# Cluster health
GET /_cluster/health

# ########
# Cat APIs (find out about the cluster)
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

```