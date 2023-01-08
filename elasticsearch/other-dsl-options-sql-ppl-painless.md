# Alternatives to the Query DSL

* There are *so many* DSLs built in to and on top of Elasticsearch.
  * They seem to be aimed at analysts
  * Developers can ignore almost all of them

# SQL or PPL in Opensearch

* SQL and PPL (Piped processing language) are the same plugin: https://github.com/opensearch-project/sql
* Can used the Query Workbench in OS dashboards to run queries
* PPL is
  * https://opensearch.org/docs/latest/search-plugins/sql/ppl/index/
  * > is a query language that lets you use pipe (|) syntax to explore, discover, and query data stored in OpenSearch

```js
// OpenSearch
POST _plugins/_sql
{
  "query": "SELECT * FROM my-index LIMIT 50"
}
```

## SQL In Elasticsearch

https://www.elastic.co/guide/en/elasticsearch/reference/7.17/xpack-sql.html

* ES has a plugin for SQL support
* HTTP POST to `/_sql` or use the included `elasticsearch-sql-cli`
* Get SQL EXPLAIN from `/_explain`

```js
// Elasticsearch
POST /_sql?format=txt
{
  "query": "SELECT * FROM library WHERE release_date < '2000-01-01'"
}

```

## Scripting: Painless (ES) vs Scripts (OS)

* Stored procedures for ES/OS
* https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-scripting-painless.html
* https://opensearch.org/docs/2.0/opensearch/rest-api/_script-apis/index/


# Runtime fields (ES only as of Nov 2022)

* ES supports "runtime fields" - fields created at query run time via a Painless script
  * https://www.elastic.co/guide/en/elasticsearch/reference/7.17/runtime-mapping-fields.html?baymax=rec&rogue=rec-1&elektra=guide
* You define the runtime field as part of your mapping
* Examples:
  * Get a day of week from a timestamp value
  * Create new field with a static value
* OS does not support runtime fields yet - RFC is https://github.com/opensearch-project/OpenSearch/issues/1133

# ESQL != SQL

* ES has a WIP (as of end 2022) thing called ESQL which is a SQL like query language (but not SQL) - details: https://www.elastic.co/blog/introduction-to-esql-new-query-language-flexible-iterative-analytics
* It does not transpile to the normal query DSL but has it's own engine built into ES.
* Aimed at data analysts