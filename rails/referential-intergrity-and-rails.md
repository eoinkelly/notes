# The best way to create model relationships in Rails

* Aside note: in rails we need DB constraints to go in one direction only so we can actually build the objects one at a time in the DB
    * => at least one of the relationships must include a 0!!!

* rails can be smart about constraints you add at the application layer so if
  you wire models up in memory correctly it will make the validations work when
  saving to the DB

rails relationship macros don't lock down a relationship
There are things that acitverecord encoruages but it prevents very little!

### Rails default: has_one <--> belongs_to

```
class Foo
  has_one :bar
end

class Bar
  belongs_to :foo
end
```

* encourages 0-1:0-1
* does *not prevent* you from having a 0-N:0-N even though you won't do that by
  default if using the normal rails relationship methods
* to make it a real 1:0-1
    * steps
        * add a unique index to the foreign key `bars.foo_id`
        * add a foreign key constraint from belong_to side to has_one side
        * add a null false constraint to the foreign key `bars.foo_id
        * be disiplined enough to not create a `Foo` without also creating a `Bar`
* this still doesn't get us to 1:1 because we have no DB way of preventing you
  from making a has_one side Object without a corresponding belongs_to

What can I do to get from 1:0-1 to 1:1 ?

Options:
1. use a rails validation somehow
2. use postgres to enforce it

## Relationship recipes

* A has exactly 1 B
    * in rails model
        * add `has_one :b`
        * add `validates :b, presence: true`
    * in migrations/schema
        * add unique index on foreign key column
        * foreign key constraint from A to B (ensure foreign key column only has valid values)
        * prevent foreign key column from being NULL (prevents 0 in the relationship)

* A has 1-N B
    * implies foreign key is in B (in rails at least)
    * in rails model
        * add `has_many :b`
        * ??? add presence validation for at least one object
    * in migrations/schema
        * add foreign key constraint: B.a_id will only have values from A.id
        * add `null: false` to foreign key column in B
        * ensure NO unique index on foreign key column

* A has 0-1 B
    * in rails model
        * add `has_one :b`
    * in migrations/schema
        * foreign key constraint from A to B (ensure foreign key column only has valid values)


==========================================

* Sources: http://www.dcs.warwick.ac.uk/~hugh/TTM/AllforOne.pdf

Hints for being clear when talking about relationships

* assume a _relationship_ has a single direction
    * => each relationship will have an _inverse relationship_ which has the opposite direction
    * don't talk/think of relationships as "bi-directonal"
    * think of a relations ship as being _from_ one table to another not _between_ tables
    * e.g.
* don't use the word "many" as it is imprecise and can mean `0..*` or `1..*`
* don't use the word "between" for relationships because it is imprecise - it makes it hard to describe them fully e.g. "there is a 1:1 between A and B" should mean that A exactly one B and B has exactly one A but it often doesn't
* relationships have a time factor
    * most relationship statements are prefixed by "At a given time..."
    * => this relationship should be true for *all* times when either side exists
        * watch out for edge cases where one side is created or deleted before the other
* be explicit about the zero case: `1-*` is not the same "many" as `0-*`
* in SQL one side has to get the foreign key but "parent" and "child" are imprecise terms so avoid them

```
An Order (row/tuple/object) has one-many LineItem (rows/tuples/objects) (the relationship from Order to LineItem)
A LineItem has exactly one Order (the inverse relationship from LineItem to Order)

Order ->1..N LineItem
LineItem ->1 Order
```

### Expressing relationships in english

* Remember a relationship has a single direction - use "from" never "between"
* When discussion how two models A and B relate talk about a pair of relationships

Use join phrases

* at most one
* exactly one
* at least one
* zero to many

```
A has JOIN_PHRASE B
B has JOIN_PHRASE A
```

Don't use vague phrases like

* "has many"
* "has one"

### Expressing relationships in diagrams

Use

* 0..1
* 1
* 0..N
* 1..N

Don't use `*` because it can mean `0..N` or `1..N` to different people

```
[A]_1..M________________0..M_[B]
```

# 10 possible relationships

There are 10 possible relationships in between models

```
1.  exactly-one <-> exactly-one
2.  exactly-one <-> zero-one
3.  exactly-one <-> one-many
4.  exactly-one <-> zero-many
5.  one-many    <-> one-many
6.  one-many    <-> zero-one
7.  one-many    <-> zero-many
8.  zero-one    <-> zero-one
9.  zero-one    <-> zero-many
10. zero-many   <-> zero-many
```

Maybe it would be clearer to think in terms of what happens during each of CRUD? e.g.

1:1 =>
* C:
    * new A and new B must created in the same transaction with deferred constraints checking
        * A must be created first because B will need the primary_key value for its foreign_key
    * if deferred constraints checking is not available then the relationship must be 1:0-1
    * it should not be possible to create an A without also creating a B
        * enforced by ??? is this enforcable???
    * it should not be possible to create an B without also creating a A
        * can be enforced by having the B foreign_key NOT NULL
* R:
    * In genreal reads to either A or B have no effect on the relationship
    * A and B can be read as one large table joined on the `A.primary_key = B.foreign_key`
* U:
    * updates to A have no effect on B
    * updates to B columns except foreign key have no effect on A
    * updates to B foreign key must only change it to a primary key from A that exists
* D:
    * Whn an A is deleted one of the following happens to B
        * the corresponding B is deleted (must happen in 1:1)
        * the foreign key column is set to NULL (can't happen in 1:1 but can in 1:0-1)

# Relationship implemnetation mechanisms

These relationships are implemented via some combination of ? mechanisms

* Rails Layer
    * ActiveRecord validations
    * ActiveRecord callbacks
* DB Layer
    * stored procedures or triggers
* SQL Layer:
    * foreign key column in one of the tables
    * foreign keys in each table that reference a join table (optionally with data of its own)
    * unique indexes
    * making columns not null
    * delete actions
        * RESTRICT = fail with error if deletion attempted, cannot be deferred
        * CASCADE = delete the dependent model too
        * NO ACTION = fail with error if deletion attempted, is default, can be deferred
        * SET NULL = set the foreign key to NULL when the referenced model is deleted
        * SET DEFAULT = set the foreign key to its default value when the refernced model is deleted



Referential integrity ensures that the relationship between rows in two tables
will remain synchronized during *all* operations on the database updates and deletes

If the DB checks constraints after every statement there are some relationships we cannot make!
Examples are:
* 1:1


QUESTION: how does it work for create? is there an order that models must be created in to not break constraints?

NOTE: the relationship doesn't fully describe what happens when one side is deleted

> Any relationship requires that the "parent" table (the one side) have a Primary (or unique) Key (PK), that uniquely identifies each row, and the "child" table (the other side) have a Foreign Key column or columns, that must be populated with values that are the same as some existing value[s] of the Primary Key in the parent table.

> If you want a one to many (1-M) relationship then the Foreign Key should be an ordinary attribute (column or columns) in the child table that can repeat (there can be many rows with the same value)

> If you want a one to one (1-1) relationship then the Foreign key should itself be a Primary Key or unique index in the child table that guarantees that there may be at most one row in the child table with that value.

Interesting:

> application-level uniqueness validations are insufficient at best, and fail completely in concurrent environments.

## Databases support not checking foreign key constraints until the end of a transaction.

In postgres you can set a "DEFERRABLE" property of foreign keys which says
whether they should be checked after each SQL statement or checked at the end
of the transaction.

From postgres docs for CREATE TABLE

> NOT DEFERRABLE
> This controls whether the constraint can be deferred. A constraint that is
> not deferrable will be checked immediately after every command. Checking of
> constraints that are deferrable may be postponed until the end of the
> transaction (using the SET CONSTRAINTS command). NOT DEFERRABLE is the
> default. Only foreign key constraints currently accept this clause. All other
> constraint types are not deferrable.
>
> DEFERRABLE INITIALLY IMMEDIATE
> DEFERRABLE INITIALLY DEFERRED
> If a constraint is deferrable, this clause specifies the default time to
> check the constraint. If the constraint is INITIALLY IMMEDIATE, it is checked
> after each statement. This is the default. If the constraint is INITIALLY
> DEFERRED, it is checked only at the end of the transaction. The constraint
> check time can be altered with the SET CONSTRAINTS command.

* RESTRICT = fail with error if deletion attempted, cannot be deferred
* CASCADE = delete the dependent model too
* NO ACTION = fail with error if deletion attempted, is default, can be deferred
* SET NULL = set the foreign key to NULL when the referenced model is deleted
* SET DEFAULT = set the foreign key to its default value when the refernced model is deleted

http://www.postgresql.org/docs/current/interactive/sql-set-constraints.html

Rails 4.2 `add_foreign_key` [oes not yet support this option](https://github.com/rails/rails/pull/17094/files) so we would have to

1. do a raw SQL migration (no big deal)
2. use `db/structure.sql` instead of `db/schema.rb`

## 1:1

Consider a system where each `User` is required to have exactly one `Address`

* it is not possible for the DB to have a true 1:1 relationship (a chicken and egg problem)
    * If the DB has a rule that a Chicken row cannot be added without a
      corresponding Egg row and another rule that an Egg row cannot be added
      without a corresponding Chicken row then it can never add any rows!
    * in reality when we say _1:1 relationship_ we mean a `1:0-1`
    * Generally the only way to enforce a true 1:1 is in code via
        * a trigger
        * stored procedure
        * rules enforced by your app code (as in Rails)
* A 1-1 relationship effectively partitions the attributes (columns) in a table
  into two tables. This is called _vertical segmentation_.
* Reasons to do this
    1. if you are sub-classing the table entities
    1. if the usage patterns on the columns in the table indicate that a few of
       the columns need to be accessed significantly more often than the rest
       of the columns.

```ruby
class User < ActiveRecord::Base
  has_one :post
end

class Address < ActiveRecord::Base
  belongs_to :user
end
```

Steps

1. Are you sure you really want two tables here? A 1:1 relationship effectively paritions a single table.
1. Decision to make: Which model gets the `belongs_to` (i.e. which DB table gets the foreign key)
    * Considerations
        * it doesn't really matter I think ????
1. make sure the foreign_key column `addresses.user_id` cannot be null (this is a required 1:1 relationship)
1. Decide what should happen to a user's address if something tries to delete the User
    * delete the address too
1. Decide what should happen to the address's user if something tries to delete an Address
    * fail to delete the address because it still has a user
1. Optional step: add `inverse_of` to all relationship macros in the models

Consider the cases

* Create
* Read
* Update
* Delete
TODO: I'm not at all sure this is complete

which gives us final code of

```ruby
UNFINISHED

# app/models/user.rb
class User < ActiveRecord::Base
  has_one :post
end

# app/models/address.rb
class Address < ActiveRecord::Base
  belongs_to :user
end

# db/migrations/2016...add_constraints_to_address.rb
class AddConstraintsToAddress < ActiveRecord::Migration
  add_foreign_key :addresses, :users, on_delete: :cascade
  change_column_null :addresses, :user_id, false
end
```







## Misc notes

```ruby
class User < ActiveRecord::Base
  has_many :posts

  # anti-pattern: doing this in AR validations
  has_many :posts, dependant: destroy

  validates :name, presence: true
end

class Post < ActiveRecord::Base
  belongs_to :user
  validates :user, presence: true
end

# db/migrations/...
class AddConstraintsToPosts < Migration
  def change
    add_foreign_key :posts, :users, on_delete: :cascade
  end
end
```

* add `dependant: destroy` to say what whould happent to the dependant models when a User is deleted
    * -- only works if rails callbacks are fired

With the foreign key in place, any operation that causes a post to point to a non-existent user will fail. It’s important to realize that a user_id of NULL is allowed, so we still need appropriate presence validations and NOT NULL constraints.

QUESTION: it implies that this would catch changes to both User and Post that - check this?


# Questions to ask when you want to make a relationship between two rails models

1. What kind of relatinoship is it?
    * 1:1
    * 1:many
    * many:many
        * rails validations do have a place in ensuring many:many relationships because SQL cannot (is that true???)
    * choose the appropriate AR relationsship class methods
1. Is it a "required" or "optional" relationship i.e. must it always exist?
    * required relationships should have a NOT NULL constraint on the foreign_key
1. is it a 1:1 relationshiop?
    * if it is a `has_one` relationship then there should be a unique index on the foreign key column
1. Which table should have the foreign key in it - this one gets the `belongs_to` side of the relationship
    * Can the foreign key ever be NULL (i.e. is the relationship optional)
    * If the foreign key can never be null (the relationship is required) then should you add a foreign key constraint
    * Add the appropriate AR validation to match the enforce constraints in the DB
1. What (if anything) should happen to the model on the other side of the relationship when a model is deleted from the DB
    * Option: add `dependent: :destroy|???` to rails model
        * -- these are implemented as callbacks so are not always triggered e.g. `delete_all` will skip them
    * Option: add cascading deletes to the foreign key constraint  in the DB
        * `add_foreign_key :posts, :users, dependent: :delete`
        * ++ DB is good at enforcing this, AR is not


Sources

* https://robots.thoughtbot.com/referential-integrity-with-foreign-keys




our "policy" on creating relationships between models is

1. Always set it NOT NULL if it is a required relationship
2. Always add a DB foreign key constraint to ensure integrity
3. Enforce logic about what happens when one model in the relationship is deleted in the DB layer by adding cascading deletes to the foreign key rather than in the AR layer via a `dependent: destroy` which is sometimes skipped.(edited)
4. Always enforce uniqueness checks with a DB index as well as an AR validation - we can't rely on the AR validation (Background: https://robots.thoughtbot.com/the-perils-of-uniqueness-validations)


I guess a summary of the overall strategy is "First ​_enforce_​ with the DB and then add the ActiveRecord sugar"

Aside: the immigrant gem provides a generator that makes migrations to add missing foreign keys https://github.com/jenseng/immigrant - could be handy for tightening up our existing projects
