# Facts & Dimensions

Sources

* https://www.thoughtco.com/facts-vs-dimensions-1019646
* https://stackoverflow.com/questions/20036905/difference-between-fact-table-and-dimension-table
* https://www.toadworld.com/platforms/sql-server/w/wiki/9546.data-warehousing-fact-and-dimension-tables



* When modelling a business process in a DB each fact represents a single
  **event** associated with the process and contains **measurement data**
  associated with that event.
* There seems (to me) to be a
    * facts = immutable (events, you don't edit existing ones, just add new ones)
    * objects = mutable state (you may edit them e.g. customer updates email address)
* facts and dimensions are arranged in a "star" formation
    * fact table in the center with some columns which are foreign keys to the ids of the dimension tables
* fact tables can grow very large
    * important to get the granularity right (you don't want to store too much)
* fact table can be JOIN'd to its dimension tables to answer questions

Various quotes about them from the sources

> While facts correspond to events, dimensions correspond to people, items, or other
> objects. For example, in the retail scenario, we discussed that purchases,
> returns, and calls are facts. On the other hand, customers, employees, items
> and stores are dimensions and should be contained in dimension tables.

> fact tables may be aggregated, whereas Dimension tables are not aggregated.

>fact tables are not supposed to be updated in place whereas Dimension tables
>could be updated in place in some cases

> Fact and dimension tables appear in a what is commonly known as a Star
> Schema. A primary purpose of star schema is to simplify a complex normalized
> set of tables and consolidate data (possibly from different systems) into one
> database structure that can be queried in a very efficient way.

> Facts, are the verb. An entry in a fact table marks a discrete event that
> happens to something from the dimension table. A product sale would be
> recorded in a fact table. The event of the sale would be noted by what
> product was sold, which employee sold it, and which customer bought it.
> Product, Employee, and Customer are all dimensions that describe the event,
> the sale.

> Fact tables contain keys to dimension tables as well as measurable facts that
> data analysts would want to examine. For example, a store selling automotive
> parts might have a fact table recording a sale of each item. The fact table
> of an educational entity could track credit hours awarded to students. A
> bakery could have a fact table that records manufacturing of various baked
> goods.
