

Available full text search technologies

* PostgreSQL
* Oracle FTS
    * Don't know much about this
* Lucene
    * Open source Java lib for full-text search
    * https://lucene.apache.org/
* Solr
    * Java wrapper for Lucene
    * Provides XML/JSON APIs

    * based on Lucene (is part of the Apache Lucene product)
* Elasticsearch
    * Java
    * based on Lucene
* Sphinx
  * http://sphinxsearch.com/
  * C++, cross platform
  * a _full text search server_
  * lets you index data in SQL databases, or files or NoSQL databases
  * search queries expressed in a variant of SQL
  * seems bit old-school and not fashionalbe now for some reason
  * dual licensed GPL2 and commerical


Ranking models

1. Vector space model https://en.wikipedia.org/wiki/Vector_space_model
1. Okapi BM25 https://en.wikipedia.org/wiki/Okapi_BM25

Criteria for choosing:

* How fast are the indexes built for your dataset?
* how good at ranking is it?
* how much management overhead is there?
* How big is index compared to the text searched?
* How good is the ranking algorithms?
* Does it have a suggester?
* How doe sit handle typos in queries?



Key features of full text search

    Stemming
    Ranking / Boost
    Support Multiple languages
    Fuzzy search for misspelling
    Accent support


    caching
    replication
    admin interface
    apis available for your language?

http://rachbelaid.com/postgres-full-text-search-is-good-enough/
