# Searching

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl.html

- returns 10 results by default
- Sends GET requests with a body
- Form is
    ```sh
    GET /_search # search all indexes in the cluster
    GET /{index name}/_search
    GET /{index name}/_doc/_search # same as above in 6+. Pre 6.x you could use types other than _doc
    ```

```bash
GET /megacorp/_doc/_search # "search lite" - return all documents of given index
GET /megacorp/_doc/_search?q=last_name:Smith # filter those documents based on a field

# exactly same as line above (but using the query DSL)
# notice this is a GET request with a body. ES authors are fine with this.
GET /megacorp/_doc/_search
{
    "query" : {
        "match" : {
            "last_name" : "Smith"
        }
    }
}

# short-hand get all records in all indexes
GET /_search

# this is the long-hand version of the '/_search' i.e. it finds all results
GET /_search
{
    "query": {
        "match_all": {}
    }
}
```

### Query lite syntax aka URI search (discouraged)

```jsonc
// title contains 'blah'
GET /movies/_search?q=title:blah

// year is 2010 or later, and title contains 'trek'
// this is fine but all those special chars need to be URL encoded (curl can handle this for you)
GET /movies/_search?q=+year:>2010+title:trek
```

- Handy for quick searches but don't use in prod
- ++ it's short and concise
- -- query params must be URL encoded - annoying for some params
- -- queries can be cryptic
- -- the query is in the URL so can be logged
- -- don't ever ever allow end users to pass in the query string - equivalent of
  SQLi

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-query-string-query.html

- seems to overlap with the `match` query a lot
    - When should you use each?
- It is tempting to let users type this syntax into your site and just pass it
  on to ES but this is bad because
    - Your users are now tied to the query_string DSL and any changes which
      might happen to it
    - You cannot easily tune queries e.g. add boosting
    - Your users can DoS you pretty easily
- `query`
    - uses a DSL to allow searches to be performed on one line because this is
      basically the JSON version of the putting the query inline in the URL via
      `q=...`
    - it gets a series of _terms_ (NB term doesn't mean exactly the same thing
      as the normal ES use of the word) and _operators_
    - term
        - can be single word or `"multiple words surrounded by quotes"`
        - matches if the term is **contained** in the field - it is not an exact
          match
    - supports wildcards
    - supports regular expressions
    - supports a unique fuziness operator `~` which uses Damerau-Levenshtein
      distance
        - `~` is shorthand for `~2` (setting the edit distance to 2)
        - `~1` is also available
        - http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance
    - supports boosting
    - supports explicit boolean operators `AND`, `OR`, `NOT`
    - supports grouping with `()`
    - examples

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

- fields = what fields to search
- default operator
    - the default boolean operator to use to combine

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

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-simple-query-string-query.html

- more limited version of `query_string` but safer for it
- doesn't return errors for invalid syntax
- is designed for the use case where you want to provide and advanced query
  syntax for users with minimal work
    - Caution: this assumes you trust your users

### Search query anatomy

- Leaf clause
    - used to compare a field against the query string
    - Queries:
        1. match_all
        1. match
        1. multi_match
        1. query_string
        1. simple_query_string
    - Filters:
        1. range
        1. term
        1. terms
        1. exists
        1. missing
- Compound clause
    - used to combine other query clauses (both leaf and other compound clauses)
    - Queries:
        1. bool
    - Filters:
        1. bool

### Queries

- Queries match with a relevance score (floating point between 0.0 -> 1.0)
- Wrapped in a `"query": { ... }` block
- You can nest filters inside queries and nest queries inside filters

#### match_all

```jsonc
{
    "query": {
        "match_all": {} //  match all documents
    }
}
```

- all results receive a neutral score of `1` because they are all equally
  relevant

#### match

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-match-query.html

- searches a single field - see `multi_match` for searching more than one field
  at once
- The query will be analysed by
    - The analyser setup for the field, falling back to the default analyser for
      the index.
        - This helps ensure that your query will be analysed in the same way
          your indexed text was
- Query text is analysed and the terms created by analysis are used to create a
  `boolean` query
    - The default operator is `OR`
    - Example:
        - When you give it a query `Hello There boo boo` it will by default
          search for `hello OR there OR boo OR boo`
- good for _full text search_ OR _exact value_ search depending on the type of
  the field:
    - full text field
        - => use the defined _analyzer_ for that field
        - it will find substrings
    - exact field e.g. number, date, boolean, _not_analyzed_ string => do an
      exact match
        - NOTE: for exact matches you probably want a filter clause (see instead
          because it will be faster and cached

```js
// { "match": { "fieldName": "field value" }}
{ "match": { "tweet": "About Search" }}
{ "match": { "age":    26           }}
{ "match": { "date":   "2014-09-01" }}
{ "match": { "public": true         }}
{ "match": { "tag":    "full_text"  }}
```

```js
// short-hand version:
GET /eoin-test-1/_search
{
  "query": { // required wrapper for all queries
    "match": { // open the "match query"
      "description": "Enterprise" // the only required param to "match" is a field name and value
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

#### match_phrase

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-match-query-phrase.html

- analyzes the given query text and creates a "phrase query"
- the phrase query matches terms up to a configurable `slop` number (defaults
  to 0)
    - transposed terms have a slop of 2
    - you can use slop to control whether the phrase has to be in the exact
      order
    - slop controls how far you are willing to let a term move from it's
      position and still be matched
- you can configure which analyzer is used
    - the default is to use whatever analyzer is set for the field and to fall
      back to the default search analyzer if not explicit field mapping set
- if you set a high slop value you can use `match_phrase` to implement proximity
  search

```json
GET /_search
{
  "query": {
    "match_phrase": {
      "message": {
        "query": "this is a test",
        "analyzer": "my_analyzer",
        // "slop": 0
      }
    }
  }
}
```

#### multi_match

- run the same query on multiple fields

```jsonc
{
    "multi_match": {
        "query": "full text search",
        "fields": ["title", "body"]
    }
}
```

#### fuzzy

TODO: more about this

```jsonc
GET /movies/_search
{
  "query": {
    "fuzzy": {
      "title": {
        "value": "intrsteller", // 2 edits required to get to correct term
        "fuzziness": "auto"
      }
    }
  }
}
```

#### prefix

- https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-prefix-query.html

```jsonc
GET /movies/_search
{
  "query": {
    "prefix": {
      "title": {
        // searching for Interstellar, "int" works but "Int" does not - why?
        "value": "int"
      }
    }
  }
}
```

#### wildcard

```jsonc
// search for Interstellar
GET /movies/_search
{
  "query": {
    "wildcard": {
      "title": "Int*"
    }
  }
}
```

#### regexp

- https://www.elastic.co/guide/en/elasticsearch/reference/7.17/query-dsl-regexp-query.html

```jsonc
GET /movies/_search
{
  "query": {
    "regexp": {
      "title.keyword": {
        "value": "Int.*r",
        "flags": "ALL"
      }
    }
  }
}
```

### Filters

- Filters match with a yes/no
- Wrapped in a `"filter": { ... }` block
- You can nest filters inside queries and nest queries inside filters

#### range

```jsonc
{
    "range": {
        "age": {
            "gte": 20,
            "lt": 30
        }
    }
}
```

#### term

- search by exact value
- `term` query does no analysis of text (does not run any analyzer) so will
  always find exact match

```jsonc
{ "term": { "age":    26           }}
{ "term": { "date":   "2014-09-01" }}
{ "term": { "public": true         }}
{ "term": { "tag":    "full_text"  }}
```

#### terms

- same as `term` but allows multiple exact match values
- can be used to do searches for accented and unaccented values

```jsonc
{ "terms": { "tag": ["search", "full_text", "nosql"] } }
```

#### exists

- roughly equivalent to SQL IS NOT NULL
- matches if the given field is not `null` in the document

```jsonc
{
    "exists": {
        "field": "title"
    }
}
```

#### missing

- roughly equivalent to SQL IS NULL
- matches if the given field is `null` in the document

```jsonc
{
    "missing": {
        "field": "title"
    }
}
```

### bool (both a Query and a Filter)

- combines other queries
- takes the following arguments:
    - must
        - clauses which must match for the document to be included
    - must_not
        - clauses which must not match for the document to be included
    - should
        - if these queries matches we increase the score
        - If there are no `must` clauses, at least one `should` clause has to
          match. However, if there is at least one `must` clause, no `should`
          clauses are required to match.
    - filter
        - these clauses **must** match but don't contribute to the score
        - useful when you don't want a particular criteria to contribute to the
          score
- a `bool` query can be nested within a `filter`, `should` of another `bool`
- combines scores together to return a single score

Bool queries can be used in two contexts

1. filtering context: does this query match yes/no
    - aka "non scoring" query
    - faster than scoring queries
2. query context: how well does this query match our documents?
    - aka "scoring query"
    - calculates how relevant each document is to the given query and gives it a
      `_score` which is then used to sort the results by relevance

You can combine both kinds of query for best performance. First filter out the
documents you don't want and then use scoring query to calculate relevance of
that subset.

```jsonc
GET /movies/_search
{
  "sort": "title.keyword",
  "query": {
    "bool": {
      "must": {
        "match": {
          "genre": "Sci-Fi"
        }
      },
      "filter": {
        "range": {
          "year": {
            "lt": 1960
          }
        }
      }
    }
  }
}
```

### Relevancy

- ES provides multiple algorithms for calculating relevancy
- The default relevancy algorithm is _Term Frequency - Inverse Document
  Frequency_ (TF-IDF)
- Term Frequency
    - How often the term appears in a given document
    - The more times a word you are looking for appears in a document the higher
      the score
- Document Frequency
    - How often a term appears in all documents in the index
- Inverse Document Frequency
    - The weight of each word is higher if the word is uncommon across documents
- You can manually "boost" the score of a particular field when searching
    - This lets you weight certain fields more than others when calculating
      relevancy

$$Rel = TF \cdot \frac{1}{DF}$$

#### Boosting

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/mapping-boost.html

You can apply a boost parameter (many/most/all ???) queries

- A boost is a floating point number used to decrease or increase the relevance
  scores of a query.
- You can use the boost parameter to adjust relevance scores for searches
  containing **two or more queries**.
- The boost is applied only for term queries (prefix, range and fuzzy queries
  are not boosted).
- Boost values are relative to the default value of 1.0.
- A boost value between 0 and 1.0 decreases the relevance score.
- A value greater than 1.0 increases the relevance score.
- Pre 5.0 you could apply boost at index time but that is deprecated now
    - Query time boost can be changed at will without re-indexing

### Fuzziness

- The _Lenvenshtein edit distance_ i.e. the number of **one character changes**
  (where a change is a substitution, insertion or deletion) needed to make one
  string be the same as another string
- Levenshtein distance accounts for (each has an edit distance of 1):
    - Substitutions of characters: `interstallar` -> `intersteller`
    - Insertions of character: `interstellar` -> `intersterllar`
    - Deletion of characters: `interstellar` -> `interstelar`
- defaults to 0
- increasing fuzziness increases the number of results you get but can decrease
  how relevant those results are
    - e.g. if I set fuzziness to 2 and search for a 2 character query then it
      will always match every record (because 2 edits can turn it into any other
      2 character string)
    - => you should probably use `"AUTO"` as the value for fuzziness
- Sources
    - https://www.elastic.co/guide/en/elasticsearch/reference/7.17/common-options.html#fuzziness
    - https://en.wikipedia.org/wiki/Levenshtein_distance
- Use-cases:
    - handling typos and misspellings in search queries
- Queries which support fuzziness:
    1. match
    2. ???
- Possible values of `fuzziness`:
    - A number between 0-2 (inclusive)
    - `"auto"` (which is shorthand for `"auto:3,6")
        - is the preferred value
    - `"auto:low,high"`
        - Generates an edit distance based on the length of the term
        - value "AUTO" is shorthand for "AUTO:3,6"
        - When the value is AUTO then for lengths
            - 0..2 must match exactly
            - 3..5 one edit allowed
            - > 5 two edits allowed

```jsonc
GET /movies/_search
{
  "query": {
    "fuzzy": {
      "title": {
        "value": "intrsteller", // 2 edits required to get to correct term
        "fuzziness": 2
        // "fuzziness": "auto"
      }
    }
  }
}
```

### Derivatives

If I search for bicycle" I want results from "cycling" "bicyclist", "bicyclists"

### Highlighting

- ES can mark sections of field values as highlighted because they are relevant
  for the search result

### Suggesters

Faster than normal queries for autocomplete use-cases

### Pagination

- The `from` query param controls where the results returned should start from.
  `from` counts from 0
- The `size` query param controls how big the returned page of results is
- Deep pagination can **kill performance** because to get page 500 ES has to
  find collect and sort the first 499 pages before it can return the result.
- You can work around this by setting an upper bound on the number of returned
  results

```jsonc
GET /movies/_search
{
  "from": 2,
  "size": 10,
  "query": {
    "match": {
      "name": "joe"
    }
  }
}

// Alternatively you can put from and size as query params in the URL
GET /movies/_search?from=2&size=10
{
  // ...
}
```

### Search as you type

#### Query-time search as you type

Implement search as you type using the query syntax e.g. create a
`match_phase_prefix` query with a high `slop` value (e.g. 10)

### Searching and aggregating across indexes

You can run search and aggregation queries against multiple indexes at the same
query

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
    GET / myindex / _search;
    ```
3. Search all documents in multiple indexes

    ```js
    // search all documents in the named indexes
    GET /myindex,otherindex,blahindex/_search

    // search all documents in all indices which begin with 'myindex'
    GET /myindex*/_search
    ```

4. Search all documents of a particular type in all indexes
    ```js
    // search the _doc type in all indexes in the cluster
    GET / _all / _doc / _search;
    ```
