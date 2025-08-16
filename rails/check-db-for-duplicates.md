```ruby
# to see if there are any duplicate public_id values in things
sql_count_all = "SELECT count(*) from things"
sql_count_distinct_public_id = "SELECT count(distinct public_id) from things"

ActiveRecord::Base.connection.execute(sql_count_all).to_a
ActiveRecord::Base.connection.execute(sql_count_distinct_public_id).to_a
```
