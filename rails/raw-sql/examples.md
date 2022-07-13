# Examples

## Example 1

Input in Rails console:

```ruby
[3] pry(main)> User.where(email: "blah")
  User Load (0.8ms)  SELECT "users".* FROM "users" WHERE "users"."email" = $1  [["email", "blah"]]
=> []
```

Wireshark export:

```yaml
# query
PostgreSQL
    Type: Parse
    Length: 66
    Statement: a1
    Query: SELECT "users".* FROM "users" WHERE "users"."email" = $1
    Parameters: 0
PostgreSQL
    Type: Sync
    Length: 4

# response
PostgreSQL
    Type: Parse completion
    Length: 4
PostgreSQL
    Type: Ready for query
    Length: 5
    Status: Idle (73)

# query
PostgreSQL
    Type: Bind
    Length: 26
    Portal:
    Statement: a1
    Parameter formats: 1
        Format: Text (0)
    Parameter values: 1
        Column length: 4
        Data: 626c6168
    Result formats: 1
        Format: Text (0)
PostgreSQL
    Type: Describe
    Length: 6
    Portal:
PostgreSQL
    Type: Execute
    Length: 9
    Portal:
    Returns: all rows
PostgreSQL
    Type: Sync
    Length: 4

# response
PostgreSQL
    Type: Bind completion
    Length: 4
PostgreSQL
    Type: Row description
    Length: 530
    Field count: 16
        Column name: id
            Table OID: 21318
            Column index: 1
            Type OID: 20
            Column length: 8
            Type modifier: -1
            Format: Text (0)
        Column name: email
            Table OID: 21318
            Column index: 2
            Type OID: 1043
            Column length: -1
            Type modifier: -1
            Format: Text (0)
        Column name: encrypted_password
            Table OID: 21318
            Column index: 3
            Type OID: 1043
            Column length: -1
            Type modifier: -1
            Format: Text (0)
        Column name: reset_password_token
            Table OID: 21318
            Column index: 4
            Type OID: 1043
            Column length: -1
            Type modifier: -1
            Format: Text (0)
        Column name: reset_password_sent

        # ... stuff removed

        Column name: otp_required_for_login
            Table OID: 21318
            Column index: 15
            Type OID: 16
            Column length: 1
            Type modifier: -1
            Format: Text (0)
        Column name: otp_backup_codes
            Table OID: 21318
            Column index: 16
            Type OID: 1015
            Column length: -1
            Type modifier: -1
            Format: Text (0)
PostgreSQL
    Type: Command completion
    Length: 13
    Tag: SELECT 0
PostgreSQL
    Type: Ready for query
    Length: 5
    Status: Idle (73)
```

Conclusions

* Rails correctly used the extended sub-protocol and sent the data value as part of the BIND step rather than baking it into the SQL for the PARSE step

## Example 2

```ruby
[4] pry(main)> User.count
  User Count (1.3ms)  SELECT COUNT(*) FROM "users"
=> 10
```

* Rails used the extended sub-protocol
* There was no data to bind


## Example 3

```ruby
[6] pry(main)> User.where("email ILIKE ?", '%exam%')
  User Load (3.0ms)  SELECT "users".* FROM "users" WHERE (email ILIKE '%exam%')
=> [#<User id: 11, email: "foo-0@example.com", created_at: "2022-07 ...
```

* Rails used the extended sub-protocol
* BUT the data string provided was baked into the SQL sent to the DB rather than being set as part of the BIND step


## Ex 4

```ruby
[7] pry(main)> User.where("email ILIKE ?", %Q{''; select 1 from users})
  User Load (1.3ms)  SELECT "users".* FROM "users" WHERE (email ILIKE '''''; select 1 from users')
=> []

# this version did exactly the same as the version just above
[8] pry(main)> val = %Q{''; select 1 from users}
=> "''; select 1 from users"
[9] pry(main)> User.where("email ILIKE ?", val)
  User Load (1.5ms)  SELECT "users".* FROM "users" WHERE (email ILIKE '''''; select 1 from users')
=> []
```

* What you see in the log is exactly what Rails sent to the DB in the PARSE message
* Rails used the extended sub-protocol
* BUT the data string provided was baked into the SQL sent to the DB rather than being set as part of the BIND step


## ex 5


```ruby
[11] pry(main)> User.where(created_at: (1.year.ago)..(1.second.ago))
  User Load (1.1ms)  SELECT "users".* FROM "users" WHERE "users"."created_at" BETWEEN $1 AND $2  [["created_at", "2021-07-12 22:18:14.991108"], ["created_at", "2022-07-12 22:18:13.991346"]]
=> [#<User id: 11, email: "foo ...
```

* What you see in the log is exactly what Rails sent to the DB in the PARSE message
* Rails used the extended sub-protocol
* Rails sent the data values as part of the BIND step

## Overall conclusions

* You can tell from the log message whether Rails inlined the data value into the SQL or whether it was able to use bound parameters at the protocol level. The log message matches what is sent in the PARSE message. If the log message doesn't ref bound params e.g. $1, $2 etc. then your SQL was inlined
* If you pass a string to where() with `?` then Rails will inline the data values into the SQL but it will attempt to quote them first.
  * => you should avoid this whenever possible because it has less SQLi protection