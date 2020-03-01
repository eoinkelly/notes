# Full text search (FTS) in Postgres

_these notes are super raw, presenter jumped around a lot, lots of work needed to tidy them_

Goal:

We are talking about search as implemented on websites and apps and creating a set of words to describe its features accurately

Search seems to be categorised based on
1. what transforms (if any) are applied to the user's query to turn it into "search queries"
1. is any filtering of results performed
1. what sorting of results is done


Kinds of search

1. Exact match
    * does the given user query exactly match some value in the database
    * query transforms: none
    * result filtering:
    * result sorting:
        * returned in the order they are found in the DB
        * can be manually sorted
1. "Substring match"
    * does the given query exist anywhere within the table(s) that are searchable
    * returns results in the order they are found
    * implemented by
        * raw SQL
1. "Full text" search
    * treats the query as "natural language" and attempts to find results in a more sophisticated way than just substring searching
    * describes a whole set of technologies

### Searching text in postgres


Text types

* `CHAR(n)`
    * fixed length type (padded with empty bytes)
    * not recommended in Postgres because it is not more efficient that `VARCHAR`
    * padding bytes are ignored by LIKE/regex expressions
* `VARCHAR(n)`
    * variable length up to limit n
* `TEXT`
    * store unlimited text
    * trailing spaces are significant for LIKE/regex expressions
* `VARCHAR`
    * alias for 'text'

Storage

* character set e.g. UTF-8
* how much space does text take up in Postgres?
    * up to limit: 1 byte + lenght-of-string-in-bytes
    * above limit: 4 bytes + lenght-of-string-in-bytes
        * Compression can reduce this
        * TOAST will put the field in separate tables if it goes over the max row length

What is text search?

Search on parts of the text
    Matching
    Substring search
    Data extraction, cleaning, mining

#### Postgres basic text search operators

https://www.postgresql.org/docs/current/functions-matching.html

* Basic SQL substring search (does the column contain the query)
    * case sensitive
        * `LIKE` (alias `~~`)
        * `NOT LIKE` (alias `!~~`)
    * case insensitive
        * `ILIKE` (alias `~~*`)
        * `NOT ILIKE` (alias `!~~*`)
* SQL regex search
    * Don't use this
    * Uses SQL defined regex matching
    * keyword is `SIMILAR TO`
* POSIX regex search (does the whole attribute match the given regex)
    * same syntax as sed, awk
    * operators
        * `~` (case sensitive)
        * `~*` (case insensitive)
        * `regexp_match(string text, pattern text)`
    * Postgres has a lot of functions for doing stuff with regex but the examples above are enough in this context.

What is "basic search text" missing?

* -- No ranking of results
* -- No concept of language
* -- Indexes on fields doen't help much

### Postgres Full text search (FTS)


Allows searches for _tokens_ in a database of _documents_

WIthout an index this will degenerate to a serial search
We want to avoid scanning through the entire documents
Techniques for criteria based matching
    NLP
Precision vs Recall


Precision
    how accurate are rsults, how many false positives
Recall
    how many results are you getting back from a query


Terminology

* Document
    * an abstraction for the idea of a "chunk of text"
    * In Postgres it is concretely implemented as a field in a row in a table
* Lexeme
    * an abstract lexical unite representing related words
        * if the document is natural language then a lexeme is the _word root_ but lexeme is more abstract than _word root_ because it can be used in situations which are not natural language
    * e.g.
        * lexeme: SEARCH
            * stands for words:
                * searched
                * searcher
* Stop words
    * words we have decided have no value for our search
    * are often very common words in natural language e.g. "in", "the", "a" but can be anything we choose
    * some _Dictionaries_ contain lists of stop words
    * removing stop words increases the _precision_ (accuracy) of your results but breaks phrase search
* Stemming
    * the process of reducing words to lexemes
    * increases the number of results (aka _recall_)
    * stemming algorithms
        1. Normalisation using dictionaries
        1. Strip prefix/suffix
        1. Automatic production rules
            * add "ing" t
        1. Lemmatisation rules
            * define how words are stemmed in each language
        1. n-gram models
            * probabilistic models which show you can reduce words
    * multilingual stemming is challenging
* Dictionary
    * a program which accept tokens as input and outputs lexemes
    * it normalises your tokens
    * reduces the size of your document
    * can use a list of stop words as part of its execution
* Cover density
    * ranks things higher if they are closer together in the document

Steps

1. Documents are parsed into classes of tokens
    * Postgres has a built-in parser
1. Tokens are converted into _lexemes_ by a _Dictionary_

Postgres

`tsvector`
    * a document
    * preprocessed
`tsquery`
    * our search query
    * normalised into lexemes

Functions

`to_tsvector()`
`to_tsquery()`
`plainto_tsquery()`
`ts_debug()`


Operators

* `@@` tsvector matches tsquery
* `||`  tsvector concatenation
* `&&`, `||`, `!!` tsquery AND, OR, NOT
* `<->`  tsquery followed by tsquery
* `@>` tsquery contains
* `<@`  tsquery is contained in


Dictionaries

* reduce the size of your `tsvector`
* can be chained: more specific -> more general

```
CREATE TEXT SEARCH DICTIONARY name (TEMPLATE = simple, STOPWORDS = english);
ALTER TEXT SEACH CONFIGURATION name ADD MAPPING FOR word WITH english_ispell, simple;
```

Ranking functions

`ts_rank`
`ts_rank_cd` (a _Cover density_ variant of `ts_rank`)

Helper functions

`ts_stat(some_ts_vector)`
    * returns stats of words founds in the given document

Indexing text in postgres

* Normal index type in Postgres is B-Tree
    * not that useful for full-text search
* GIN index
    * inverted index: one entry per lexeme
    * Large, slow toupdate => use on less dynamic data
    * applied on `tsvector` columns
    * can also be used on jsonb columns
    * makes the `@@` fast on tsvector
* GIST
    * lossy index (produces false positives
    * slower because PG has to go back to the row and remove false postivies
    * better on fewer unique items
    * can be applied on `tsvector` and `tsquery` columns
    * makes the `@@` fast on tsvector
    * makes the `<@` and `@>` fast on tsquery

In PG 12 you can create a generated tsvector column from the text column(s) and then put an index on that
    ??? is there some advantage to this?

Useful postgres modules

* pg_trgm
    * trigram indexing operations
* unaccent
    * a dictionary which removes accents and diacritics
* fuzzystrmatch
    * string simlarity
        * Levenshtein distances (most common)
        * other algorithms (which may not work well iwth UTF-8 - see docs)
            * Soundex
            * Metaphone
            * Double metaphone
    * `SELECT name FORM users WHERE levenshtein('Stephen', name) <= 2;`
        * finds "Stephen" and "Steven"

Other index types

* VODKA
* RUM
    * Lexeme positional information stored
    * Faster ranking
    * Faster phrase search
    * makes `<=>` fast for finding distance between timestamps, floats, money

Examples

```
postgres@postgres=#  select to_tsvector('A nice day for a car ride');
            to_tsvector
-----------------------------------
 'car':6 'day':3 'nice':2 'ride':7
(1 row)

-- the above are the lexemes generated by to_tsvector

Time: 43.485 ms
postgres@postgres=#  select plainto_tsquery('I am riding');
 plainto_tsquery
-----------------
 'ride'
(1 row)

Time: 6.045 ms
postgres@postgres=#  select to_tsvector('A nice day for a car ride') @@ plainto_tsquery('I am riding');
 ?column?
----------
 t
(1 row)

Time: 6.831 ms

postgres@postgres=#  select plainto_tsquery('I am riding a bike');
 plainto_tsquery
-----------------
 'ride' & 'bike'
(1 row)

Time: 4.859 ms
postgres@postgres=#  select to_tsvector('A nice day for a car ride') @@ plainto_tsquery('I am riding a bike');
 ?column?
----------
 f
(1 row)

Time: 4.081 ms

postgres@postgres=#  select websearch_to_tsquery('"The stray cats" - "cat shelter"');
             websearch_to_tsquery
----------------------------------------------
 'stray' <-> 'cat' & !( 'cat' <-> 'shelter' )
(1 row)

Time: 6.720 ms
```

Postgers has websearch_to_tsquery which attempts to do a "google style" query


### Free text but not natural language

* Idenitify arbitrary strings e.g. keywords in device logs
* Dictionaries are not very helpful here
* you'll probably use `LIKE '%thingicareabout%'`


Trigrams

* n-gram model: a probabilistic language model (Markov chains)
* 3 chars -> trigram
* the similarity of two pieces of alphanumeric text is given by the number of shared trigrams
* GIN indexes can use trigram operations to index text
    * works with LIKE and regexes
    * warning: the index can be bigger than the table!

```
postgres@postgres=#  create extension pg_trgm;
CREATE EXTENSION
Time: 159.161 ms
postgres@postgres=#  select show_trgm('eoin kelly');
                         show_trgm
-----------------------------------------------------------
 {"  e","  k"," eo"," ke",ell,eoi,"in ",kel,lly,"ly ",oin}
(1 row)

Time: 7.418 ms
```

Trigram tricks

similarity(text, text) -> real
text <-> text => Distance (1-similarity)
text % text => true if over similarity_threshold


These are supported indexes
    * GIN
    * GiST is efficient: use k-nearest neighbour (k-NN) ???


Postgres character set support

```
pg_client_encoding() -- find out what the client is using
convert(string bytea, src_encoding name, dest_encoding name) -- convert between encodings
convert_from
convert_to

SET CLIENT_ENCODING TO 'value'; -- setup automatic character set conversion
```

Collation in PostgreSQL

* sort order and character classification
* different in different languages
* can be defined per column
* can be defined per operation
* The DB has LC_COLLATE and LC_CTYPE but you are not restricted by them
* PG 12 introduced nondetermistic collations (ignore accents when sorting text)

JSON

* can be converted into tsvectors
* `jsonb_to_tsvector()`
* PG12 has the _Official SQL/JSON query language_
    * ???
* JsQuery addon
    * JSONB query language iwth GIN support
    * equivalent to `tsquery`
    * https://github.com/postgrespro/jsquery


Maintenance

* Use `VACUUM ANALYSE`
    * keeps table stats up to date
    * keeps GIN indexes up to date
* ALTER TABLE SET STATISTICS
    * keeps table stats accurate (number of distinct values, correlated columns)
* `maintenance_work_mem` is very important because GIN, GiST indexes are often huge



