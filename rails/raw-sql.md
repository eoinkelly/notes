# Rails and raw SQL

## find_by_sql

* `SomeModel.find_by_sql("sql string")`
* alwasy returns an array
* will _instantiate_ ActiveRecord object for each result


## select_all

`SomeModel.connection.select_all("sql string")`

Note: lives on the connection adapter, not the model itself. You can get at the
instance of the connection adapter class via any model `User.connection`

Returns an intance of `ActiveRecord::Result` which can be converted to an array of hashes

Just runs a SQL query and gives back data - it does not create ActiveRecord
objects from that data. This makes it very useful for building reports if we
don't need to do "Active Model" things with the returned data.

```
[34] pry(main)> things = Page.connection.select_all('SELECT * from pages LIMIT 5')
   (0.4ms)  SELECT * from pages LIMIT 5

[33] pry(main)> things.class
ActiveRecord::Result < Object

things.first # returns a hash
things.first.class # Hash
things.to_a
```

## Aside: %q and %Q refresher

* Use %q or %Q to simplify quoting
* Using raw SQL probably belongs as a class method on your model

%q = create a string using "single quote rules" but you can choose your own
delimiter

%Q = create a string using "double quote rules" but you choose your own
delimiter i.e. use %Q if you need variable interpolation

```
[22] pry(main)> x = 34
34
[23] pry(main)> %q[this is literally #{x}!]
"this is literally \#{x}!"
[24] pry(main)> %Q[this is literally #{x}!]
"this is literally 34!"
[25] pry(main)>
```
