# Ecto

Background

- Data flow is `{your app} <--> {ecto} <--> {ecto adapter} <--> {database}`
- Think of Ecto as a suite of tools, not one tool
- It can be used for sending raw SQL to a DB and getting back lists & tuples

Ecto is split into 6 modules

1. Repo
    - represents the "repository"
    - **all** communicaiton with the database passes through this module's
      functions - if you want to talk to the database you have to talk to the
      Repo.
    - Unlike ActiveRecord, you don't do things to the in-memory model and have
      SQL happen behind the scenes
    - you can do **all basic CRUD** operations with just `Repo` alone.
    - the Repo has one job: send data to and from the database.
    - you don't call methods directly on `Repo`, instead you create a module in
      our app (usually called `MyApp.Repo` but can be named anything!) and pull
      in the `Repo` functions via `use Ecto.Repo`
        - why?
            - `use` allows us to configure a module by passing in the name of an
              OTP app with database credentials and access details. Otherwise we
              would have to pass in DB details to each function call.
    - Low level functions tend to end in `_all` e.g.
        - `all`
        - `insert_all`
        - `delete_all`
        - `update_all`
    - Low level functions don't deal in schema objects - they return tuples
2. Query
    - can build layered queries (bit like rails scopes, LINQ queries)
3. Schema
    - syntax sugar for creating a struct
    - lets you define transformations to get between table and struct
    - you are not locked into a 1:1 relationship between tables and struts
        - => a table can have many associated struts
        - => can a strut have more than one table
4. Changeset
    - The schema is sugar for creating a struct. You can set any value you want
      to things within a struct but they will be rejected when you try to save
      the record to the DB. For this reason you should always make changes to
      your schema structs using a `changeset` method, which knows about the
      validations and casts required to satisfy both the DB and the data model.
    - holds all chagnes you want to perform on a DB
    - encapsulates
        - receiving external data
        - casting types
        - validating
        - writing to DB
5. Multi
    - allows you to create multiple changes to a DB (opens a transaction)
6. Migration
    - syntax sugar for changing the structure of the DB

```elixir
# Repo.aggregate("table_name", :aggregate_func_name, :column_name)
Repo.aggregate("artists", :count, :id)

Repo.insert(schema_instance)
Repo.delete(schema_instance)
Repo.get_by(SchemaModule, attr_name: attr_value)
Repo.update(changeset_instance)
```
