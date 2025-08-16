# MySQL Cheat sheet

```sh
# mysql: connect as <current_user>@localhost to ??? database
mysql

# mysql: connect as <current_user>@localhost user to database `some_db`
mysql some_db

# mysql: connect as root@localhost user to database `some_db`
mysql -u root some_db
```

```bash
### show connection info
\s

### Get help
\?

### list all databases
SHOW DATABASES;

### quit
\q
quit
exit

### change database to `some_db`
USE some_db;

### show all tables in current database
SHOW TABLES;

### create database
???

### Describe a table
DESCRIBE some_table;
```

# See the name and size on disk of each relation (table)

# See total database size on disk

# See the name and size on disk of each index

# Find the biggest indexes in a DB

# Drop a database

```sql
-- sql
DROP DATABASE foo;
```
