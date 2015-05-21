
find(query_type, query_ptions)

query_type = :css (default) or :xpath

A common pattern for

* fill_in
* select
* check

etc. is that their locators are

1. name
2. id (without #)
3. label


fill_in does use find() under the hood so it will magically wait
