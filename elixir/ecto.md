

Query language
    * can build layered queries (bit like rails scopes, LINQ queries)

Changesets

* holds all chagnes you want to perform on a DB
* encapsulates
    * receiving external data
    * casting types
    * validating
    * writing to DB


Repository pattern



Data flow is

    {your app} <--> {ecto} <--> {ecto adapter} <--> {database}
