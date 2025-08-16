```bash
be rake dynamoid:ping
be rake dynamoid:create_tables

be ruby ./seed.rb

be ruby ./repl.rb

# pry commands
pry> Dynamoid.adapter.list_tables
pry> Dynamoid.adapter.delete_table("eoin_dynamoid_users")
```
