# Elasticsearch

## Questions

* is it a good practice to disble dynamci fields on all indexes in most cases?

## Sources

* Elasticsearch in Action (but out of date but very good IMHO)
    * up to
* Official ES docs

## Installing

* Install via Docker - it's easiest

## Setup

### Logging

* Default log level is INFO
* Changing log level to DEBUG is **very** noisy
* Instead, turn on logging of each index, fetch and query operation
* The logs appear in docker-compose output as you would hope.

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

## Overview

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

* A node is an instance of ES
* node exposes Restful JSON API on port 9200
* nodes form a cluster on port 9300 (also what the java APIs use)
* ES is a document database
    * documents have fields
* by default every field in a document is indexed in an inverted index i.e. it is searchable
* uses JSON as the serialization format for documents
* storing a document in ES is called "indexing the document"
* A cluster is a group of nodes with the same value of `cluster_name` (you can see this value by visiting in a browser http://localhost:9200/)
* Provides a Java API has two kinds of clients
    1. Node client
        * your client joins the cluster as a "non data" node
        * it can forward requests to the node which does have the data
        * uses port 9300 and the native ES transport protocol
    1. Transport client
        * lighter weight
        * just forwards requests to the cluster
        * uses port 9300 and the native ES transport protocol
* ES supports YAML in request and response bodies - add `?format=yaml` to your request
* You can configure your index settings to not store `_source`
    * Doing this in combination with using an external ID means you could use ES just for the searching of the index and get a list of database IDs as results.
    * You would then get those rows from the DB
    * Is this a good pattern?
        * ++ keeps the index size smaller (which only matters if you have very large source documents)
        * -- you have to do more work to get results
* A lot of the constraints of ES are actually constraints from Lucene
* The part of ES that writes data to disk is called the "gateway"
* ES is described as "schema free" or "schemaless"
    * It's kinda bs
    * That means the docs are not _bound_ by a schema, it doesn't mean there is no schema

### Default ports

* TCP 9200 for Restful API queries
* TCP 9300 for inter-node communication or "transport"
    * They Java API connects to this port
* TCP 5601 for Kibana
  * Kibana connects to ES server over 9200

### Things it isn't good at

* ES does not support transactions - you should use something else if you need them
* ES doesn't work as well as a store if you have many frequent writes
* ES doesn't do great at modelling data with lots of complex relationships


### Elasticstack

* Elasticsearch
    * Search API based on Lucene
    * Features
      * documented oriented (schemaless) DB
      * near real time
      * distributed
      * search and analytics engine
      * categorised under NoSQL
* Kibana
    * Web UI for elasticsearch
    * provides management and dev tools for elasticsearch
    * lets you build visualisations
    * http://localhost:5601
* X-pack
    * provides
        * monitoring,
        * reporting
        * alerting,
            * send alerts to slack etc. based on the result of some search e.g. too many failed login attempts
        * security,
            * provides authentication and authorization to your cluster or documents and even fields within it
        * graphing
            * visualise your data as a connected graph rather than a flat set of data files in documents
            * includes a _Graph API_ and some UI within Kibana
        * machine learning
            * finds anomalies in time series data (unsupervised learning)
    * prior to 6.3 it required registration and downloading separate code for some features. After 6.3 you get all code when you download elasticsearch but some parts require commerical license
* Logstash
    * centralised daemon which has many plugins to allow ingesting logs from various places, filtering them, transforming them and sending them to various places including elasticsearch
    * has 200+ plugins
* Beats
    * a collection of components built on top of a core `libbeat`
    * capable of collecting files from disk, metrics from OS, metrics from specific binaries
    * the client-side to logstash's server-side
    * installed on machines which generate the logs
* Elastic cloud
    * fully hosted Elastic stack provided by elastic.co

## Cat APIs

https://www.elastic.co/guide/en/elasticsearch/reference/current/cat.html

* Intended for humans to consume
* Returns tabular text not JSON
* All cat APIs are under `GET /_cat/...`
* Handy query params
  * `v` => show headings in tables
  * `help` => show help output describing each column instead of actual output
* you can control which columns are returned by query param
* some options to control the presentation of numeric and time columns
* can sort by different columns
* output available in the following formats
    * text (default)
    * JSON `format=json` (add `pretty` to get pretty output via CURL, Kibana is always pretty)
    * YAML `format=yaml`
    * cbor `format=cbor`
        * Concise Binary Object Representation
        * a binary format loosely based on JSON
        * https://en.wikipedia.org/wiki/CBOR
    * smile `format=smile`
        * a binary encoding of JSON
        * called smile because the data header includes `:)`
        * https://en.wikipedia.org/wiki/Smile_(data_interchange_format)

## JSON API Overview

* Responses are unformatted JSON unless you pass `?pretty=true`
    * Kibana formats all responses by default

### Searching and aggregating across indexes

You can run search and aggregation queries against multiple indexes at the same query

1. Search all documents in all indexes
    ```js
    // search all documents in all indexes
    GET /_search
    GET /_search?size=10 // same as query above, default size is 10

    GET /_search?size=100
    ```
2. Search all documents in one index
    ```js
    // search all documents in just the myindex index
    GET /myindex/_search
    ```
3. Search all documents of one type in an index
    ```js
    // search all documents in just the _doc type in just the myindex index
    GET /myindex/_doc/_search
    ```
4. Search all documents in multiple indexes
    ```js
    // search all documents in the named indexes
    GET /myindex,otherindex,blahindex/_search
    ```
5. Search all documents of a particular type in all indexes
    ```js
    // search the _doc type in all indexes in the cluster
    GET /_all/_doc/_search
    ```

## Documents (CRUD)

### C: Index a document

* use PUT when you have a document id you want to use.
* use POST to autogenerate a document id
    * Autogenerated IDs are 20 character long, URL-safe, Base64-encoded GUID strings.

Examples

```bash
# PUT {index name}/{type name}/{chosen document id}
PUT /website/_doc/1
{
    "title": "My second blog entry",
    "text":  "Still trying this out...",
    "date":  "2014/01/01"
}

# POST will autogenerate an document ID
# POST {index name}/{type name}
POST /website/_doc/
{
    "title": "My second blog entry",
    "text":  "Still trying this out...",
    "date":  "2014/01/01"
}

# PUT {index name}/{type name}/{document id}
curl -XPUT 'localhost:9200/megacorp/_doc/1?pretty' \
  -H 'Content-Type: application/json' \
  -d'{ "first_name" : "John",
       "last_name" :  "Smith",
       "age" :        25,
       "about" :      "I love to go rock climbing",
       "interests": [ "sports", "music" ] }'

# response
{
  "_index": "megacorp",
  "_type": "_doc",
  "_id": "1",
  "_version": 1,
  "result": "created",
  "_shards": {
    "total": 2,
    "successful": 1,
    "failed": 0
  },
  "_seq_no": 0,
  "_primary_term": 1
}
```

### R: Read a document

```sh
# Form is GET /{index name}/{type name}/{document id}
curl -XGET "http://elasticsearch:9200/megacorp/_doc/1"

{
  "_index": "megacorp",
  "_type": "_doc",
  "_id": "1",
  "_version": 1,
  "found": true,
  "_source": {
    "first_name": "John",
    "last_name": "Smith",
    "age": 25,
    "about": "I love to go rock climbing",
    "interests": [
      "sports",
      "music"
    ]
  }
}
```

### U: Update a document

* You update by POSTing to the `_update` type of the index.
* It increments the `_version` of the document created for you by ES
* You can do an UPSERT by passing `doc_as_upsert` param
* You can pass small executable scripts as text values in your JSON and ES will execute them to get the value
    * examples
        * calculate a new value based on the existing value of the field
        * calculate a new value based on another field in the document

```bash
# Do an UPDATE
# POST /{index_name}/_update/{document_id}
POST /mythings/_update/1
{
  "doc": {
    "price": "28.99"
  }
}


# Do an UPSERT
POST /mythings/_update/1
{
  "doc": {
    "price": "28.99"
  },
  "doc_as_upsert": true
}
```

### D: Delete

You can delete documents by id

```bash
DELETE /mythings/_doc/123
```

## Aggregation

* As well as FTS, ES can be used for data analytics
* Example use-cases
    * most popular blog tags
    * average poplularity of a certain group of posts
    * average popularlity of posts for each tag

TODO

## Searching

* returns 10 results by default
* Sends GET requests with a body
* Form is
    ```sh
    GET /_search
    GET /{index name}/_search
    GET /{index name}/{type name}/_search # Pre 6.x only
    ```

```bash
# Kibana console

GET /megacorp/employee/_search # "search lite" - return all documents of given index and type

GET /megacorp/employee/_search?q=last_name:Smith # filter those documents based on a field

# exaclty same as line above (but using the query DSL)
# notice this is a GET request with a body. ES authors are fine with this.
GET /megacorp/employee/_search
{
    "query" : {
        "match" : {
            "last_name" : "Smith"
        }
    }
}

# short-hand get all records
GET /_search

# this is the long-hand version of the '/_search' i.e. it finds all results
GET /_search
{
    "query": {
        "match_all": {}
    }
}
```

### Search query anatomy

* Leaf clause
  * used to compare a field against the query string
  * examples:
    * match
    * match_all
    * range
    * term
    * terms
    * exists
    * missing
* Compound clause
  * used to combine other query clauses (both leaf and other compound clauses)
  * examples:
    * `bool`

#### match_all
{ "match_all": {}} # match all documents
    * all resutls receive a neutral score of `1` because they are all equally relevant

#### match

https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-match-query.html

```js
// short-hand version:
GET /eoin-test-1/_search
{
  "query": { // required wrapper for all queries
    "match": { // open the "match query"
      "description": "Enterprise" // the only require param to "match" is a field name and value
    }
  }
}


// long-hand version of the same query
GET /eoin-test-1/_search
{
  "query": {
    "match": {
      "description": { // <-- name of field to query
        "query": "Enterprise", // this text gets analysed
        "analyzer": "blah", // defaults to the analyzer defined for this field at index time, falls back to the default analyzer for the index
        "fuzziness": 0 // defaults to 0, increase value to be tolerant of misspellings and finding similar words
        "operator": "and", // change default operator from OR to AND
        "lenient" true, // ignore exceptions caused by data-type mismatches
      }
    }
  }
}
```

* searches a single field - see `multi_match` for searching more than one field at once
* The query will be analysed by
    * The analyser setup for the field, falling back to the default analyser for the index.
        * This helps ensure that your query will be analysed in the same way your indexed text was
* Query text is analysed and the terms created by analysis are used to create a `boolean` query
    * The default operator is `OR`
    * Example:
        * When you give it a query `Hello There boo boo` it will by default search for `hello OR there OR boo OR boo`
* good for _full text search_ OR _exact value_ search
* how it functions depends on they type of the field:
    * full text field
        * => use the defined _analyzer_ for that field
        * it will find substrings
    * exact field e.g. numger, date, boolean, _not_analyzed_ string => do an exact match
        * NOTE: for exact matches you probably want a filter clause instead because it will be faster and cached

```js
// { "match": { "fieldName": "field value" }}
{ "match": { "tweet": "About Search" }}
{ "match": { "age":    26           }}
{ "match": { "date":   "2014-09-01" }}
{ "match": { "public": true         }}
{ "match": { "tag":    "full_text"  }}
```

#### multi_match

* run the same query on multiple fields

{
    "multi_match": {
        "query":    "full text search",
        "fields":   [ "title", "body" ]
    }
}

#### range

{
    "range": {
        "age": {
            "gte":  20,
            "lt":   30
        }
    }
}

#### term

* search by exact value
* `term` query does no analysis of text (does not run any analyzer) so will always find exact match

    { "term": { "age":    26           }}
    { "term": { "date":   "2014-09-01" }}
    { "term": { "public": true         }}
    { "term": { "tag":    "full_text"  }}

#### terms

* same as `term` but allows multiple exact match values
* can be used to do searches for accented and unaccented values

    { "terms": { "tag": [ "search", "full_text", "nosql" ] }}

#### exists

* roughly equivalent to SQL IS NOT NULL
* matches if the given field is not `null` in the document

    {
        "exists":   {
            "field":    "title"
        }
    }

#### missing

* roughly equivalent to SQL IS NULL
* matches if the given field is `null` in the document

    {
        "missing":   {
            "field":    "title"
        }
    }

#### query_string

https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html

* seems to overlap with the `match` query a lot
    * When should you use each?
* It is tempting to let users type this syntax into your site and just pass it on to ES but this is bad because
    * Your users are now tied to the query_string DSL and any changes which might happen to it
    * You cannot easily tune queries e.g. add boosting
    * Your users can DoS you pretty easily

* `query`
    * uses a DSL to allow searches to be performed on one line because this is basically the JSON version of the putting the query inline in the URL via `q=...`
    * it gets a series of _terms_ (NB term doesn't mean exactly the same thing as the normal ES use of the word) and _operators_
    * term
        * can be single word or `"multiple words surrounded by quotes"`
        * matches if the term is **contained** in the field - it is not an exact match
    * supports wildcards
    * supports regular expressions
    * supports a unique fuziness operator `~` which uses Damerau-Levenshtein distance
        * `~` is shorthand for `~2` (setting the edit distance to 2)
        * `~1` is also available
        * http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance
    * supports boosting
    * supports explicit boolean operators `AND`, `OR`, `NOT`
    * supports grouping with `()`
    * examples
        ```sh
        # where the status field contains "active"
        status:active

        # where the title field contains either "quick" or "brown"
        title:(quick OR brown)

        # where book.author field contains "Josh Smith"
        book.author:"Josh Smith"

        # any field in book which contains "Joan"
        book.\*:"Joan"

        # where the title field has any non-null value
        _exists_:title
        ```

* fields = what fields to search
* default operator
    * the default boolean operator to use to combine
```json
{
    "query_string": {
        "fields": [
            "title"
        ],
        "default_operator": "AND",
        "query": "bassoon",
        "boost": 10,
        "fuzziness": "AUTO"
    }
},
```

#### simple_query_string

https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html

* more limited version of `query_string` but safer for it
* doesn't return errors for invalid syntax
* is designed for the use case where you want to provide and advanced query syntax for users with minimal work
    * Caution: this assumes you trust your users

#### bool

* combines other queries
* takes the following arguments:
    * must
        * clauses which must match for the document to be included
    * must_not
        * clauses which must not match for the document to be included
    * should
        * if these queries matches we increase the score
        * If there are no must clauses, at least one should clause has to match. However, if there is at least one must clause, no should clauses are required to match.
    * filter
        * these clauses **must** match but don't contribute to the score
        * useful when you don't want a particular criteria to contribute to the score
* a `bool` query can be nested within a `filter`, `should` of another `bool`
* combines scores together to return a single score

Queries can be used in two contexts

1. filtering context: does this query match yes/no
    * aka "non scoring" query
    * faster than scoring queries
2. query context: how well does this query match our documents?
    * aka "scoring query"
    * calculates how relevant each document is to the given query and gives it a `_score` which is then used to sort the results by relevance

You can combine both kinds of query for best performnace. First filter out the documents you don't want and then use scoring query to calculate relevance of that subset.

### Relevancy

* ES provides multiple algorithms for calculating relevancy
* The default relevancy algorithm is _Term Frequency - Inverse Document Frequency_ (TF-IDF)
* Term Frequency
    * The more times a word you are looking for appears in a document the higher the score
* Inverse Document Frequency
    * The weight of each word is higher if the word is uncommon across documents
* You can manually "boost" the score of a particular field when searching
    * This lets you weight certain fields more than others when calculating relevancy

#### Boosting

https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping-boost.html

You can apply a boost parameter (many/most/all ???) queries

* A boost is a floating point number used to decrease or increase the relevance scores of a query.
* You can use the boost parameter to adjust relevance scores for searches containing **two or more queries**.
* The boost is applied only for term queries (prefix, range and fuzzy queries are not boosted).
* Boost values are relative to the default value of 1.0.
* A boost value between 0 and 1.0 decreases the relevance score.
* A value greater than 1.0 increases the relevance score.
* Pre 5.0 you could apply boost at index time but that is deprecated now
    * Query time boost can be changed at will without re-indexing

### Fuzziness

Sources

* https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#fuzziness
* https://en.wikipedia.org/wiki/Levenshtein_distance


Use-cases:

* handling typos and misspellings in search queries

Querys which support fuzziness:

1. match
2. ???

Overview

* interpreted as the _Lenvenshtein edit distance_ i.e. the number of one character changes needed to make one string be the same as another string
* defaults to 0
* increasing fuzziness increases the number of results you get but can decrease how relevant those results are e.g. if I set fuzziness to 2 and search for a 2 character query then it will always match every record (because 2 edits can turn it into any other 2 character string)
    * => you should probably use `"AUTO"` as the value for fuzziness

Values

* A number between 0-2 (inclusive)
* `"AUTO"`, `"AUTO:low,high"`
    * is the preferred value
    * Generates an edit distance based on the length of the term
    * value "AUTO" is shorthand for "AUTO:3,6"
    * When the value is AUTO then for lengths
        * 0..2 must match exactly
        * 3..5 one edit allowed
        * >5 two edits allowed

### Derivatives

If I search for bicycle" I want results from "cycling" "bicyclist", "bicyclists"

### Highlighting

* ES can mark sections of field values as highlighted because they are relevant for the search result

### Suggesters

Faster than normal queries for autocomplete use-cases

## Indexing

A logical grouping of related types and documents

index ~= database

* Since 7.0 an index can only contain a single type
  * i.e. the "database" can only contain one "table"
  * so index and type are 1:1 in ES7+

it seems in the examples they use `_doc` for the type e.g. `users/_doc` and `products/_doc`
_doc is the "default type of an index in 6+

Indexes are auto-created the first time you push a document into it
You can also create an index before-hand with an "index template" it lets you control the defaults of the index e.g. num shards, type mappings etc.

ES creates an "inverted index" for FTS fields i.e. it stores (conceptually at least) tuples of the form:

    (term, frequency, [document_ids])

This index is the basis of ES

### Create index

```js
// Create an index
PUT /eoin_test_1
{
  "settings": {
    "index": {
      "number_of_shards": 5,
      "number_of_replicas": 2
    }
  }
}
// => response
{
  "acknowledged": true,
  "shards_acknowledged": true,
  "index": "eoin_test_1"
}
```

#### ES6 and earlier

Create an index with a named mapping. This syntax only works in ES6 and older. ES7 only allows one type per index and the default type name is `_doc`
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

#### ES7+

In ES7+ you cannot name the type
```js
PUT /eoin_test_2
{
  "settings": {
    "index": {
      "number_of_shards": 5,
      "number_of_replicas": 2
    }
  },
  "mappings": {
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
// =>
{
  "acknowledged": true,
  "shards_acknowledged": true,
  "index": "eoin_test_2"
}
```

### Types

A type consists of

1. a name
2. a mapping
    * describes the fields/properties that this object may have (i.e. defines a schema for the object)

### Analyzers

* The job of an analyzer is to take the value of a text field and break it into "terms".
* These terms then become the keys of the inverted index.

Analyzers run
1. when you add a document to the index but they also run
    * called _Index analyzer_
2. on the query string(s) when you preform a query.
    * called _Search analyzer_

In most cases you want the same analyser at both index and search times but there are times when you want the flexibility to choose a different analyser at search time.
    I'm assuming it would be slower than using the analyser which created the index???

Different text fields in the same document can have different analysers

An analyzer is the name given to a chain of 3 kinds of function:

1. Character filter (0 or more)
    * tidy up a string before it is tokenized
2. Tokenizer (exactly 1)
    * tokenize the string
3. Token filter (0 or more)
    * filters the tokens in various ways

```
{document-field-or-query-char-stream}
    |> [char-filter-1, ...] -> {[char, char, char, ...]}
    |> [Tokenizer] -> {[start-offset-in-stream, token, end-offset-in-stream], ...}
    |> [token-filter-1, ...] -> {[term1, term2, term3, ...]}
```

#### Built-in character filters

* `mapping`
    * replace substrings within the stream
* `html_strip`
    * remove html tags and decode HTML entities
* `pattern_replace`
    * replaces with regex

Can I create custom char filters?
Note: the tokenizer will see the **output** of the character filter chain

#### Built-in Tokenizers

https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-tokenizers.html

Tokens take a character stream and create tokens
A token is roughly equivalent to a word

There are a few categories of built-in tokenizers

* Word oriented tokenizers
    1. `standard`
        * splits input text on word boundaries and removes most punctuation
        * the most poplular
        * suitable for most languages
        * discards punctuation and whitespace characters
    1. `letter`
        * divides text into terms whenever it gets a character which is not a letter
    1. `lowercase`
        * like `letter` but also lowercases everything
    1. `uax_url_email`
        * like `standard` but recognises URLs and email addresses as single tokens
    1. `whitespace`
        * splits text on whitespace only
    1. `classic`
        * Grammar based tokenizer for English
    1. `thai`
        * splits Thai text into words
* Partial word tokenizers
    1. `ngram`
        * creates n-grams of character
        * defaults to min = 1, max =2 i.e. it creates character unigrams and digrams
    1. `edge_ngram`
* Structured text tokenizers
    1. `keyword`
        * outputs exactlty the string it received
    1. `pattern`
        * split text on a matching regex
    1. `char_group`
    1. `simple_pattern`
    1. `simple_pattern_split`
    1. `path_heirarchy`

You can test tokenizers pretty easily (this also lets you see their output quite directly)

```js
// Use this to invoke the analyzer configured as the default for this index
POST /myindex/_analyze
{
  "text": "This is 66! Blah-blah.text $% foobar FooB"
}

// Use this form to use the analyzer defined for a given field in a given index
POST /eoin_test_2/_analyze
{
  "field": "f1",
  "text": "This is 66! Blah-blah.text $% foobar FooB http://blah.com and the mail is eoin@foo.com"
}

// Use this to invoke any analyzer
POST _analyze
{
  "tokenizer": "standard", // <-- choose a tokenizer from the list above
  "text": "This is 66! Blah-blah.text $% foobar FooB"
}
// response
{
  "tokens" : [
    {
      "token" : "This",
      "start_offset" : 0,
      "end_offset" : 4,
      "type" : "<ALPHANUM>",
      "position" : 0
    },
    {
      "token" : "is",
      "start_offset" : 5,
      "end_offset" : 7,
      "type" : "<ALPHANUM>",
      "position" : 1
    },
    {
      "token" : "66",
      "start_offset" : 8,
      "end_offset" : 10,
      "type" : "<NUM>",
      "position" : 2
    },
    {
      "token" : "Blah",
      "start_offset" : 12,
      "end_offset" : 16,
      "type" : "<ALPHANUM>",
      "position" : 3
    },
    {
      "token" : "blah.text",
      "start_offset" : 17,
      "end_offset" : 26,
      "type" : "<ALPHANUM>",
      "position" : 4
    },
    {
      "token" : "foobar",
      "start_offset" : 30,
      "end_offset" : 36,
      "type" : "<ALPHANUM>",
      "position" : 5
    },
    {
      "token" : "FooB",
      "start_offset" : 37,
      "end_offset" : 41,
      "type" : "<ALPHANUM>",
      "position" : 6
    }
  ]
}
```

#### Built-in token filters

Token filters can add, remove or change tokens
* ES has approx. 48 built-in token filters - see the menu on https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-apostrophe-tokenfilter.html

* `lowercase`
    * convert token to lowercase
* `stop`
    * removes "stop words" i.e. common words which have little impact on search relevance e.g. the, and, is, an
* `asciifolding`
    * removes diacritics
* `ngram`
    * suitable for partial matching or autocomplete
* `edge_ngram`
    * suitable for partial matching or autocomplete
    * similar to `ngram` but only outputs ngrams that start at the beginning of a token
* `truncate`
    * truncate long tokens

#### Built-in analyzers

1. Standard
    * divides text into termss on word boundaries
    * "word boundaries" are defined by _Unicode Text Segmentation_ algorithm
    * removes most punctiation, lowercases terms and supports removing stop words
2. Language specific analyzers
    * e.g. `english`, `french`
3. Whitespace
    * just creates terms by splitting on whitespace
4. Simple
    * lowercases everything
    * creates a new term whenever it encounters a character which is not a letter
5. Stop
    * Same as simple but allows you to remove stop words
6. Keyword
    * a noop analyzer, just returns it's input as a single term
7. Pattern
    * splits into terms based on regex
    * supports lower-casing and stop words
8. Fingerprint
    * creates a fingerprint which can be used for duplicate detection

If the analyzers above don't work for you, you can create your own "custom" analyzer.

##### Standard analyzer (the default)

The default analyzer for full-text fields is `standard` - it is good for western languages.

The `standard` analyzer is

1. Character filter
    * none
2. Tokenizer
    1. `standard` tokenizer
        * divides text into termss on word boundaries
        * "word boundaries" are defined by _Unicode Text Segmentation_ algorithm
        * removes most punctiation, lowercases terms and supports removing stop words
3. Token filters
    1. `standard` token filter
        * in theory tidies up tokens emitted from the tokenizer but currently does nothing
    2. `lowercase` token filter
        * converts all tokens to lowercase
    3. `stop` token filter
        * removes "stop words" i.e. common words which have little impact on search relevance e.g. the, and, is, an
        * by default the stopword list is set to `_none_` so this filter does nothing unless you configure it to do so.

#### Creating a custom analyzer

```js
// Example of creating a custom analyzer
ngram_search_analyzer: {
    type: "custom" // tells ES we are creating a new kind of analyzer not configuring an existing one
    char_filter: ["html_strip"] // char filters, can be omitted
    tokenizer: "standard", // choose your tokenizer
    filter: ["truncate_filter", "lowercase", "asciifolding"], // token filters, can be omitted
}
```

### Normalizers

https://www.elastic.co/guide/en/elasticsearch/reference/current/analysis-normalizers.html

* A chain of char filters into token filters
* A normalizer is like an analyzer but it can only emit one token

You canonly only use token filters that take one character at a time input

```
{document-field-or-query-char-stream}
    |> [char-filter-1, ...] -> {[char, char, char, ...]}
    |> [token-filter-1, ...] -> {[term1, term2, term3, ...]}
```
### Mappings

#### Dynamic mapping

https://www.elastic.co/guide/en/elasticsearch/reference/current/dynamic-field-mapping.html

* All ES indexes have a schema but it is dynamic.
* ES is not really "schemaless" but it is capable of dynamically **creating** or **adding** to a schema as you add documents - it will not change or remove an existing field in a schema
* You can customise how dynamic field mapping happens with _Dynamic templates_
* Dynamic mapping is on by default but can be configured to ignore new fields (`dynamic: false`) or throw an exception on new fields (`dynamic: "strict"`)

```js
PUT my_index
{
  "mappings": {
    "dynamic": false, // ignore new fields added by documents
    "properties": {
      "user": {
        "properties": {
          "name": {
            "type": "text"
          },
          "social_networks": {
            "dynamic": true, // allow documents creates to create new fields under `social_networks`
            "properties": {}
          }
        }
      }
    }
  }
}
```

The rules for how JSON types are converted are what you would expect but "string" is a bit more complext:

1. If the string looks like a date then save it as on
2. If the string looks numeric then save it as number
3. Otherwise generate an `my_field` analysed field and a `my_field.keyword` keyword subfield.

#### other
```
# view the mapping of an index
GET /indexname/_mapping
GET /indexname/typename/_mapping

? how to get mappings for multiple indexes?
```

You should create types within an index to match the _common_ fields in your doucments e.g. log lines

https://www.elastic.co/guide/en/elasticsearch/reference/current/mapping.html

When defining a mapping we seem to need both an "analyzer" and a "search analyzer"

This example creates two logical properties on the docment

1. `my_field` - a FTS enabled chunk of text
2. `my_field.keyword` an unparsed version of the raw string
```json
// I think the shape of this is a default for ES when you give it a string value
// in JSON and don't have a mapping defined

// this defines a property `my_field` indexed as both a 'text' type for FTS and
// as a 'keyword' type for sorting, aggregation and filtering
"my_field": {
  "type": "text",
  "fields": {
    "keyword": {
      "type": "keyword",
      "ignore_above": 256
    }
  }
}
```

* `_all`
    * is a catch all field
    * is a meta-field on a **type**
    * can be enabled or disabled

you can add dynamic fields to an index (which are calculated based on stored fields)

A mapping defines

* whether string fields should be treated as "full text" or not
    * `text` type => full text
    * `keyword` type => exact matches only
* the format of date values
* rules creating dynamic fields

Since ES 6.0 an index can contain only one type

A mapping has 2 kinds of fields

1. meta-fields on a _document_
    * define some metadata about the document's metadata is treated
    * examples
        * `_index`
            * the name of the index this document is in
        * `_type`
            * the type of this document
        * `_id`
            * the id of the document
        * `_source`
            * seems to be all the properties of the document i.e. most stuff is in here
2. fields aka properties


Fields

* each field has a data type, one of
* the same field can be indexed multiple times for different purposes - this is called _multi-fields_
    * the `fields` paramter to a property defines multi-fields

Field data types

* scalar
    * string
        * `text` (analysed for full text search)
        * `keyword` (not FTS analysed, supports sorting, filtering, aggregations)
    * Numeric
        * `byte`
        * `short`
        * `integer`
        * `long`
        * `float` (IEEE 754 32 bit)
        * `half_float` (IEEE754 with 16bit precision)
        * `scaled_float` (float backed by a long and fixed scaling factor)
            * useful for storing prices - use scaling factor of 100
                * I'm dubious about that statement - prices should probably be ints of cents
        * `double` (IEEE 754 64 bit)
    * `date`
    * `boolean`
    * `binary`
    * Range
      * `integer_range`
      * `float_range`
      * `long_range`
      * `double_range`
      * `date_range`
    * `ip`
        * store IPv4 and v6 addresses
* Complex data types
    * `array`
        * all elements must be same type
    * `object`
        * allows inner objects within the JSON document
    * `nested`
        * supports arrays of inner objects where each object needs to be independently queryable
            * Question not sure what that means?
* special
    * `geo_point`
        * stores lat and long
    * `geo_shape`
        * store geometric shapes
    * `completion`

? array, binary object,

Limiting the number of mappings which can be created

"Mapping explosion" can be caused if you insert a bunch of documents which have very different shapse. You can set some params to control this:

* `index.mapping.total_fields.limit`
    * The maximum number of fields in an index.
    * The default value is 1000.
* `index.mapping.depth.limit`
    * The maximum depth for a field, which is measured as the number of inner objects. For instance, if all fields are defined at the root object level, then the depth is 1. If there is one object mapping, then the depth is 2, etc.
    * The default is 20.
* `index.mapping.nested_fields.limit`
    * The maximum number of nested fields in an index, defaults to 50. Indexing 1 document with 100 nested fields actually indexes 101 documents as each nested document is indexed as a separate hidden document.
    * Default is 50

You don't need to define your fields before-hand but you can create an explicit mapping when you create your index
You can add fields to an existing index too.
Existing type and field mappings **cannot be changed** because it would invalidate existing documents in the index

> Fields and mapping types do not need to be defined before being used.
> Thanks to dynamic mapping, new mapping types and new field names will be
> added automatically, just by indexing a document.

### Aliases

* you can alias an index or indexs
* creates a level of indirection between your users and your indexes
    * useful to allow you to reindex without breaking your users

## Physical layout

### Decisions you make when you create an index

Sources

* https://thoughts.t37.net/designing-the-perfect-elasticsearch-cluster-the-almost-definitive-guide-e614eabc1a87 (this is very good)
* https://www.elastic.co/blog/how-many-shards-should-i-have-in-my-elasticsearch-cluster

Overview

* Decisions you can change later
    * Choose number of nodes in cluster
    * Choose number of replica shards for each primary shard
* Decisions you cannot change later
    * Choose number of primary shards

* Your first design will probably suck because you don't know what the workload will be
    * => you will have to iterate a few times
    * questions you won't yet know the answers to
        * how many queries/sec?
        * does your cluster's workload need a lot of CPU, memory or both?
            * influences whether to go with a few large hosts or more smaller hosts
        * what kinds of queries?
        * how fast do indexes grow?
* You should have an odd number of master nodes to avoid split-brain
* lucene creates lots of small files - choose a file system which can handle this - inodes etc.
* during a merge lucene makes copies of all segments before changing them so your shard can't be more than 0.5 your free disk space
    * a merge is when lucene consolidates two smaller segments into one
* shard size
    * 50GB shard should be your max (it's not an enforced max)
    * don't make them too small because small shards have many small segment files
    * aim for shards in the 20GB - 50GB range
* ES can scale to many, many nodes

Advice from the t37.net blog post

> less 3M documents: 1 shard
> between 3M and 5M documents with an expected growth over 5M: 2 shards.
> More than 5M: int (number of expected documents / 5M +1)

```js
// recommended settings to get better logs from slower queries
PUT /index/_settings
{
    "index.search.slowlog.threshold.query.warn: 1s",
    "index.search.slowlog.threshold.query.info: 500ms",
    "index.search.slowlog.threshold.query.debug: 1500ms",
    "index.search.slowlog.threshold.query.trace: 300ms",
    "index.search.slowlog.threshold.fetch.warn: 500ms",
    "index.search.slowlog.threshold.fetch.info: 400ms",
    "index.search.slowlog.threshold.fetch.debug: 300ms",
    "index.search.slowlog.threshold.fetch.trace: 200ms"
}
```

### Index

* each index is stored on disk as the a single set of files (fields, mapping, settings)
* index refreshes are
    * where ES checks for new documents to index in the index
    * expensive so it happens once per second by default
        * ES is _near real-time_ not _real-time_


### Clusters

A cluster

* hosts 1+ indexes
* provides operations like searching and indexing
* is 1+ nodes
* nodes try to join a cluster called `elasticsearch` by default
    * you should edit the cluster name to ensure your node doesn't accidentaly join another cluster

### Nodes

* An ES cluster is a collection of nodes running on different machines
* each node has
  * a unique ID
  * a unique name
      * can be assigned by the `config/elasticsearch.yml` config file
* a node is a single instance of the elstic search process
* a node is always part of a cluster, even if it is the only node in the cluster
* nodes try to join a cluster called `elasticsearch` by default
    * you should edit the cluster name to ensure your node doesn't accidentally join another cluster
* nodes on the same network will automatically find each other

Question: how do nodes find other nodes? do they spam the network on 9200 or 9300?

### Shards

* Physically it is a directory of files where Lucene stores data for your index
* A shard is the smallest unit of data that can be moved between nodes
* A single index can be split across multiple nodes i.e. it has some documents on each node
* The process of dividing an index across nodes is called _sharding_
* Once an index is created the **number** of shards cannot be modified
    * can the location of shards be modified?
* When you query, ES will collect results from all shards on all nodes
* WHen you search an index
    * ES will search a complete set of shards but they can be a mix of primary and replica
* A shard is a _Lucene index_
    * A directory of files containing an inverted index
    * => An _Elasticsearch index_ is made up of multiple _Lucene indexes_ (shards)
* When you index a document the following things get stored
    1. The document itself (provided your index uses the default to store `_source`)
    2. Term dictionary
        * maps terms to document ids
        * My guess is that searches only need to look in the term dictionary and not at the documents themselves
    3. Term frequencies
        * quick access to the number of appearance of a term in a document
        * used to calculate relevancy


#### Primary shards

* When you index a document it is first sent to one of the primary shards in the index
    * That shard is chosen at random based on a hash of the document's ID
    * The shard could be on a different node to the one you connected to
    * Once the primary shard has written the new document it sends it to its replicas to keep them in sync

#### Replica shards

* shards can be duplicated on other nodes (replica shards) to provide high availabilty
* replica shards are extra copies of the "primary" shard
* replica shards are promoted to primary shard when the node holding the primary shard fails
* All read operations .e.g. query operations and aggregations can be executed on replica shards too!
    * Only write operations must go to the primary shard I guess?
* Useful for both increasing search performance (searchs hit a mix of primary and replica shards) and fault tolerance

### Get cluster info and health

What does `?pretty` arg do?

```
# kibana developer console example queries
GET /
GET _cluster/health
GET _count
```

```bash
curl -XGET "http://localhost:9200/?pretty"
# {
#   "name": "FNAhp-l",
#   "cluster_name": "elasticsearch_eoinkelly",
#   "cluster_uuid": "5E5Aa-tVQxeMkb9SUQAtqA",
#   "version": {
#     "number": "5.5.0",
#     "build_hash": "260387d",
#     "build_date": "2017-06-30T23:16:05.735Z",
#     "build_snapshot": false,
#     "lucene_version": "6.6.0"
#   },
#   "tagline": "You Know, for Search"
# }


curl -XGET "http://localhost:9200/_cluster/health"
# {
#   "cluster_name": "elasticsearch_eoinkelly",
#   "status": "yellow",
#   "timed_out": false,
#   "number_of_nodes": 1,
#   "number_of_data_nodes": 1,
#   "active_primary_shards": 30,
#   "active_shards": 30,
#   "relocating_shards": 0,
#   "initializing_shards": 0,
#   "unassigned_shards": 30,
#   "delayed_unassigned_shards": 0,
#   "number_of_pending_tasks": 0,
#   "number_of_in_flight_fetch": 0,
#   "task_max_waiting_in_queue_millis": 0,
#   "active_shards_percent_as_number": 50
# }
```





