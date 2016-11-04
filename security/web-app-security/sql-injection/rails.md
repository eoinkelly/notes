# Practical mitigation of SQLi in Ruby and Rails

Key takeaways for avoiding SQLi in my work

1. Use prepareed statements always
1. Input scrubbing is hard to get right
    * they can encode as hex in UTF-8 or other text encoding

```ruby
# Demonstrate a basic SQLi attack
u = "foo@bar.com"
pass = "secret1"

ActiveRecord::Base.connection.select_all("SELECT * FROM users WHERE email='#{u}' AND password='#{pass}'").to_a
# (0.2ms)  SELECT * FROM users WHERE email='foo@bar.com' AND password='secret1'
# => [{"id"=>1, "email"=>"foo@bar.com", "password"=>"secret1", "created_at"=>"2016-10-19 17:37:17.554655", "updated_at"=>"2016-10-19 17:37:17.554655"}]


pass = "' OR 1=1 --"
ActiveRecord::Base.connection.select_all("SELECT * FROM users WHERE email='#{u}' AND password='#{pass}'").to_a
# (0.2ms)  SELECT * FROM users WHERE email='foo@bar.com' AND password='' OR 1=1 --'
# => [{"id"=>1, "email"=>"foo@bar.com", "password"=>"secret1", "created_at"=>"2016-10-19 17:37:17.554655", "updated_at"=>"2016-10-19 17:37:17.554655"},
# {"id"=>2, "email"=>"blah@bar.com", "password"=>"secret2", "created_at"=>"2016-10-19 17:37:26.810786", "updated_at"=>"2016-10-19 17:37:26.810786"}]

# does same as above
pass = "' OR '1'='1' --"
# presenters favour this so it might be more compatibile across databases
```

## Prepared statements in Rails and Postgres

* since Rails 3.1 ActiveRecord has used prepared statements for its queries
* prepared statements are enabled by default for Postgres
* I'm unsure about MySQL - they were not enabled by default for a long time because of perf problems

```ruby
ActiveRecord::Base.connection                   # => instance of ActiveRecord::ConnectionAdapters::PostgreSQLAdapter
ActiveRecord::Base.connection.raw_connection    # => PG::Connection

connection = ActiveRecord::Base.connection.raw_connection

# connection.prepare("statement_name", "statement_SQL_with_positional_placeholders")
# connection.exec_prepared("statement_name", ["array", "of", "values", "to", "fill", "positional", "args", "with")

conn.prepare("statement1", "insert into table1 (id, name, profile) values ($1, $2, $3)")
conn.exec_prepared("statement1", [ 11, "J.R. "Bob" Dobbs", "Too much is always better than not enough." ])


# TODO: how to use prepared statement with
ActiveRecord::Base.connection.select_all
# def select_all(arel, name = nil, binds = [])

```
