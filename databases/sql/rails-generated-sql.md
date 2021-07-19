TODO: this should probably be moved once I know what it is

TODO: document how to implement each possible kind of relationshipo in rails
    show what the models look like and the db schema

Do all these even make sense?
How many of these can Rails do?

1. 0-1 <--> 0-1
1. 1 <--> 1
1. does something like 1 <--> 0-1 exist?
1. 0-N <--> 0-N
1. 0-N <--> 0-1
1. N <--> 1
more ???

## Q: How does ActiveRecord handle dupes?

It does a lot of single query tables to get stuff
It does join when you do a has_many through

## Rails has_many

```ruby
class Owner < ApplicationRecord
  has_many :dog
end

class Dog < ApplicationRecord
  belongs_to :owner
end
```

```sql
-- join order doesn't matter for INNER JOIN
-- owners is the has_many side
-- dogs is the belongs_to side

-- In an INNER JOIN on this kind of relationship, the has_many side can have repeated rows, the belongs_to side cannot

select  * from owners inner join dogs on owners.id = dogs.owner_id order by owners.id;
--  id | name |      email       | id |  name  | owner_id
-- ----+------+------------------+----+--------+----------
--   1 | john | john@example.com |  1 | shep   |        1
--   1 | john | john@example.com |  2 | brandy |        1
--   2 | jane | jane@example.com |  3 | fido   |        2
--   2 | jane | jane@example.com |  4 | scooby |        2
--   3 | bill | bill@example.com |  5 | mitzy  |        3
--   5 | mike | mike@example.com |  7 | woof   |        5
-- (6 rows)
```


The relationship is defined by the combination of

1. the has_*/belongs_to line in each model
2. the db schema of the belongs_to
   * it should **always** have a foreign key setup
   * it should probably also have an index, which may be unique if that fits for your use-case

Rails does create the index and the foreign key but doesn't make it a unique index by default
What does having a unique index mean?
    it means that every record in the table must be linked to a different record in the other table
    => the tables are extensions of each other, seems like they shoudl maybe be the same table, when does this occur?

The migration which creates the table which holds the belongs_to should setup an index and a foreign key relationship

```ruby
# use a non-unique index if the other side is a has_many
t.belongs_to :author, index: true, foreign_key: true

# use a unique index if the other side is a has_one
t.belongs_to :supplier, index: { unique: true }, foreign_key: true
```
## Rails has_one

almost the same as has_many except the db schema sets up a unique index