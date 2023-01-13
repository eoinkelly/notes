

# MySQL & MariaDB character sets and collations

If you set the charset then the default collation for that charset will be used

```sql
-- show at the server level
SHOW VARIABLES LIKE 'collation_server';
SHOW VARIABLES LIKE 'character_set_server';

-- change at the server level (presumabely for just this session). If you want to change for all sessions, change my.cnf
SET character_set_server = 'latin2';
SET collation_server = 'latin2_czech_cs';


-- show at the database level
SHOW CREATE DATABASE mydb;

-- change at the database level
CREATE DATABASE czech_slovak_names
  CHARACTER SET = 'keybcs2'
  COLLATE = 'keybcs2_bin';
ALTER DATABASE czech_slovak_names COLLATE = 'keybcs2_general_ci';


-- show at the table level
SHOW CREATE TABLE mydb;

-- change at table level
CREATE TABLE english_names (id INT, name VARCHAR(40))
  CHARACTER SET 'utf8'
  COLLATE 'utf8_icelandic_ci';
ALTER TABLE table_name
 CONVERT TO CHARACTER SET charset_name [COLLATE collation_name];


-- show at the table level
SHOW CREATE TABLE mydb;

-- change at column level
CREATE TABLE european_names (
  croatian_names VARCHAR(40) COLLATE 'cp1250_croatian_ci',
  greek_names VARCHAR(40) CHARACTER SET 'greek');
```

The modern choice: `utf8mb4_general_ci` collation in `utf8mb4` charset

## Versions

> utf8mb4, available since MySQL 5.5.3.

* MySQL 5.7
    * default charset is latin1
    * default collation is latin1_swedish_ci
* MySQL 8.0
    * changed default charset and collation to `utf8mb4` and `utf8mb4_0900_ai_ci`
        * can be overridden in `my.cnf`
        * might be overriden if the server was upgraded from 5.7 or older

* MariaDB 10.4 docker: `latin1` and `latin1_swedish_ci`
* MariaDB 10.5 docker: `utf8mb4` and `utf8mb4_general_ci`
* MariaDB 10.6 docker: `utf8mb4` and `utf8mb4_general_ci`
* MariaDB 10.8 docker: `utf8mb4` and `utf8mb4_general_ci`

It _seems_ like the default MariaDB character set is still `latin1` but that some distros will change that (source: https://mariadb.com/kb/en/character-set-and-collation-overview/). The official mariadb docker image changed the default in 10.5

I think the TL;DR is that you can't rely on the server or database defaults

## Rails

The key place to choose utf8mb4 in Rails in your `config/database.yml`. If it
is set there **before** you create your database or run migrations then you
will get `utf8mb4` as a default for everything.

```yaml
# config/database.yml
production:
  adapter:  mysql2
  database: thing_dev
  username: db_user
  password: *****
  host: localhost

  encoding: utf8mb4
  collation: utf8mb4_unicode_ci
```

You can specify the charset as an option to `create_table` if you can't change it for the whole DB

```ruby
create_table "users", options: "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4", force: :cascade do |t|
  # ...
end
```

### Upgrading a MySQL db to utf8mb4

### Gotcha: column length change

> That changes the maximum length a column or index can hold. So if a column was a varchar(256) in utf8, it should now be a varchar(191) in utf8mb4.

https://jibai31.wordpress.com/2017/05/04/how-to-choose-your-mysql-encoding-and-collation/
https://railsmachine.com/articles/2017/05/19/converting-a-rails-database-to-utf8mb4.html suggests it might not be straightforward to convert existing tables


