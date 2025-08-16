# Rails batches API

## Gotchas

Using UUID as the type of the id column breaks these methods. You can write your
own.

## Sources

- https://api.rubyonrails.org/v6.1.3.1/classes/ActiveRecord/Batches.html

## Overview of API

- `MyModel.find_each`
    - fetches them into memory one batch at a time
    - yields records to the block one record at a time
    - use when you want to deal with each record individually
- `MyModel.find_in_batches`
    - yields an `Array` of objects (length set by batch_size param)
    - use when you want to have the batch as an array specifically and not an
      ActiveRecord::Relation
- `MyModel.in_batches`
    - yields an `ActiveRecord::Relation` object which represents all the records
      in the batch
    - use when you want to call AR methods like `destroy_all` etc.
