# Full-text search

## Available full text search technologies

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

## Ranking models

1. Vector space model https://en.wikipedia.org/wiki/Vector_space_model
1. Okapi BM25 https://en.wikipedia.org/wiki/Okapi_BM25

## Criteria for choosing between search engines

* How fast are the indexes built for your dataset?
* how good at ranking is it?
* how much management overhead is there?
* How big is index compared to the text searched?
* How good is the ranking algorithms?
* Does it have a suggester?
* How fast do results return?
* How doe sit handle typos in queries?

## Key features of "do what I mean" full-text search

1. Return results quickly
2. Return results in the order of most releveant to least relevant (Ranking)
    * Boost: Allow users to add weight to certain query terms at query time
    * SQL allows you to sort results based on some value in the dataset but it doesn't support the abstract notion of "relevance" out of the box
3. Correct common misspellings of query terms
    * Also called "fuzzy search"
4. Autosuggest completions for partial query terms
5. Recognise query terms that are synonyms of each other and treat them as the same (Synonyms)
6. Recognise words which are linguistic variations of each other and treat them the same (Stemming)
    * e.g. cats, catlike, catty are relevant to the term 'cat'
7. Handle phrases correctly
    * i.e. allow user to search for multiple terms in a given sequence
8. Remove common words such as "a", "an", "of" (in English) (Stop words)
9. Support pagination of results
10. Support multiple languages
11. Support searching within search results i.e. narrowing down a search interatively
    * Also called "faceted search"
    * Allows users to find things even when they don't know exactly what they are looking for


Where does Accent support fit into this list?



