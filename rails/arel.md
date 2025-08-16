# Arel

Possibly pronounced "eh-rel"

Sources

- http://radar.oreilly.com/2014/03/just-enough-arel.html
- https://github.com/rails/arel/blob/master/lib/arel/predications.rb
- https://github.com/rails/arel

## ActiveRecord query interface #where

Higher level API than Arel

```
where(name: 'x')
# WHERE name = 'x'

where(name: nil)
# WHERE name IS NULL

where(name: ['x', 'y'])
# WHERE name IN ('x', 'y')

where(name: ['x', 'y', nil])
# (WHERE name IN ('x', 'y') OR name IS NULL)
```

- #where
    - is a DSL for making a SQL WHERE clause
    - can only combine queries using AND
    - it only provides the `=` (equal), `<>` (not equal) and `in` operators
    - cannot do numeric comparisons e.g. `<=`

- #where can receive as arg
    1. a hash
    2. an array
    3. a string
    4. an arel object

Usage of the AR query interface should be limited to models in the system!

SQL things Arel can do _ named functions _ Arel::Nodes::NamedFunction #new(name,
expr, aliaz = nil) _ order by _ all conditionals/comparisons \* Unions

The layers:

1. ActiveRecord Query DSL
2. Arel
3. Raw SQL

Arel is

- just a SQL generator library - it does not call the DB or retrieve data
- the engine that rails uses to turn method calls (ActiveRecord Query DSL) into
  SQL statements.
- Each column in Arel::Table is represented by Arel::Attributes::Attribute

How you work with Arel

1. Figure out what I want to do in raw SQL
2. As much as possible express it with the normal ActiveRecord query DSL.
   Express the rest as Arel
3. Get hold of a reference to Arel::Table
4. Get a reference to some Arel::Attributes::Attribute (columns) in it
5. send predicate messages to the Arel::Attributes::Attribute to start building
   the query
6. Start work on the query using ActiveRecord #where

Example

```
SELECT posts.*
  FROM posts
 WHERE posts.published_at > (now() - INTERVAL '1 year')
    OR posts.title LIKE '%updated';


Post.where(
  Post.arel_table[:published_at].gt(1.year.ago).
    or(Post.arel_table[:title].matches('%updated')))



# Use this method if you are not in ActiveRecord (you can use Arel with any DB connection)
authors = Arel::Table.new(:authors)
authors.where(authors[:name].eq('amy'))
# it seems #where is also part of Arel ???


# Either A or B
Author.where(city: 'Orlando').where('name = ? OR name = ?', 'Bob', 'Carol')

name =Author.arel_table[:name]
Author.where(city: 'Orlando').where(name.eq('Bob').or(name.eq('Carol')))


# One of a list SQL "IN"
Author.where(city: 'Orlando').where('name IN (?)', ['Bob', 'Carol']) # raw

name =Author.arel_table[:name]
Author.where(city: 'Orlando').where(name.in(['Bob', 'Carol'])) # arel
Author.where(city: 'Orlando').where(name: ['Bob', 'Carol']) # AR query interface (best)

# BETWEEN
Author.where('book_count BETWEEN ? AND ?, 1, 5)

book_count = Author.arel_table[:book_count]
Author.where(book_count.between(1..5))

Author.where(book_count: 1..5) # will call Arel #in under the covers (best way)
```

What SQL calls "SELECT clause" is called a "projection" in Arel

Arel is designed to let you make SQL without the problems associated with string
interpolation. It can be used (as Rails does) to build an ORM.

Conclusion: to make the most of Arel I first need to be good at SQL

Advantages of Arel over raw SQL:

1. It automatically scopes column names if you have a join e.g. `id` would be
   ambigious if you join two tables which have `id` columns
1. It lets you break SQL queries apart to be more readable
1. It is harder to arbitrarily combine sub queries that use string interpolation

Arel::Table _ acts like a hash that has each column of the table as a key _ each
column is of type Node

Arel has "predications". A predication is the logical affirmation of something
about another; especially : assignment of something to a class "i contend that
thing A is related to thing B by {predicate}"

Arel's predications are documented at
https://github.com/rails/arel/blob/master/lib/arel/predications.rb

When breaking apart a query to use Arel, I find a good rule of thumb is to break
out a method

1. anywhere the word AND or OR is used,
2. or when something is wrapped in parenthesis.

Arel is a Relational Algebra gem that allows you to generate SQL queries
directly from an abstract syntax tree (AST) of nodes. It’s what Active Record
uses internally to build up expressions symbolically from the Hash syntax.

QUESTION: What exacty is a relational algebra

The #where method in Active Record can accept an Arel node e.g.
`User.where({arel methods})`

```
User.arel_table

[3] pry(main)> pat = Page.arel_table
#<Arel::Table:0x007ffa5e8d8320 @name="pages", @engine=Page (call 'Page.connection' to establish a connection), @columns=nil, @aliases=[], @table_alias=nil, @primary_key=nil>

[8] pry(main)> pat.class.ancestors
[
    [0] Arel::Table < Object,
    [1] Arel::FactoryMethods,
    [2] Arel::Crud,
    [3] Object < BasicObject,
    [4] PP::ObjectMixin,
    [5] ActiveSupport::Dependencies::Loadable,
    [6] JSON::Ext::Generator::GeneratorMethods::Object,
    [7] Kernel,
    [8] Buff::Extensions::Kernel::Reporting,
    [9] BasicObject
]

# Basic usage

pat = Page.arel_table
query = pat.project(Arel.sql('*'))
query.to_sql
 "SELECT * FROM \"pages\""

[16] pry(main)> Page.where(pat[:header].eq('foo')).to_sql
"SELECT \"pages\".* FROM \"pages\"  WHERE \"pages\".\"header\" = 'foo'"
```

The ancestor list for Arel::Table is not too crazy

Arel provides a bunch of predicates:

- eq
- not_eq
- in
- not_in
- gteq
- gt
- lteq
- lt
- matches
- does_not_match

can append `*_all` or `*_any` to all the above

```ruby
# match any row where name is equal to 'value'
name.eq('value')

# eq_all does not make much sense I think - the *_all suffix makes more sense for predicates like `in`
# combines the predicates with AND
name.eq_all(['val1', 'val2', 'val3'])

# "match any row where name is one of these values
# combines predicates with OR
name.eq_any(['val1', 'val2', 'val3'])

[2] pry(main)> usert = User.arel_table
[6] pry(main)> name = usert[:name]

[7] pry(main)> usert.where(name.eq('joe')).to_sql
"SELECT FROM \"users\"  WHERE \"users\".\"name\" = 'joe'"

[9] pry(main)> usert.where(name.eq_all(['joe', 'bob', 'dog'])).to_sql
"SELECT FROM \"users\"  WHERE (\"users\".\"name\" = 'joe' AND \"users\".\"name\" = 'bob' AND \"users\".\"name\" = 'dog')"

[10] pry(main)> usert.where(name.eq_any(['joe', 'bob', 'dog'])).to_sql
"SELECT FROM \"users\"  WHERE (\"users\".\"name\" = 'joe' OR \"users\".\"name\" = 'bob' OR \"users\".\"name\" = 'dog')"


[14] pry(main)> usert.where(name.in_any([[1,2], [3,4]])).to_sql
"SELECT FROM \"users\"  WHERE (\"users\".\"name\" IN (1, 2) OR \"users\".\"name\" IN (3, 4))"

[15] pry(main)> usert.where(name.in_all([[1,2], [3,4]])).to_sql
"SELECT FROM \"users\"  WHERE (\"users\".\"name\" IN (1, 2) AND \"users\".\"name\" IN (3, 4))"
```

The Squeel library provides some sugar on top of Arel

```
Author.where { name ~= 'foo' }
```
