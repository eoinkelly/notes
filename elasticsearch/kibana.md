## Kibana discover panel

To see stuff in "Discover" you have to

1. Create an _Index pattern_ under _Management > Stack Management_.

- When you create an Index pattern you are prompted to choose one timestamp
  field which will be used by Kibana for showing you the time graph and allowing
  time based filtering of results
- You can make one index pattern as default which will make it the default for
  the Discover panel.

The search box can use:

1. Kibana Query Language (KQL) which is the default
    - https://www.elastic.co/guide/en/kibana/7.10/kuery-query.html
1. Lucene query mini language
    - This is the "legacy" option
    - https://www.elastic.co/guide/en/kibana/7.10/lucene-query.html
    - This option may eventually be removed from the GUI
    - It doesn't support nested objects which KQL does
    - When this option is selected you can also enter queries using the
      Elasticsearch query DSL

> _Note_ Neither KQL nor Lucene query mini-language is the same thing as the
> Elasticsearch query DSL.

## Kibana saved objects

Types

1. Visualisation object
    - Appear under the "Visualise" tab in Kibana
2. Dashboard
    - Appear under the "Dashboard" tab in Kibana
3. Index pattern

They are presented as multiple JSON blobs in the "Saved Objects" UI. Q: where
are they saved? In ES or in browser?
