-- ******************************************
-- ******************************************
-- Create a true 1:1 relationship in Postgres
-- ******************************************
-- ******************************************

-- clean up previous runs of this script
ALTER TABLE addresses DROP CONSTRAINT addresses_user_fk;
ALTER TABLE users DROP CONSTRAINT users_address_fk;
DROP TABLE IF EXISTS addresses;
DROP TABLE IF EXISTS users;

-- create the tables
CREATE TABLE users
(
  id INTEGER NOT NULL primary key,
  email CHARACTER VARYING NOT NULL,
  address_id INTEGER NOT NULL
);

CREATE TABLE addresses
(
  id INTEGER NOT NULL PRIMARY KEY,
  city CHARACTER VARYING NOT NULL,
  user_id INTEGER NOT NULL
);

-- add deferrable constraints
ALTER TABLE addresses ADD CONSTRAINT addresses_user_fk FOREIGN KEY (user_id) REFERENCES users DEFERRABLE INITIALLY IMMEDIATE;
ALTER TABLE users ADD CONSTRAINT users_address_fk FOREIGN KEY (address_id) REFERENCES addresses DEFERRABLE INITIALLY IMMEDIATE;

-- add unique indexes to prevent 1 becoming many
CREATE UNIQUE INDEX addresses_user_id_uniq ON addresses (user_id);
CREATE UNIQUE INDEX uses_address_id_uniq ON users (address_id);

-- insert data deferring all constraints checking to the end of the transaction
BEGIN;
SET CONSTRAINTS ALL DEFERRED;
INSERT INTO users VALUES (1, 'foo@bar.com', 10);
INSERT INTO addresses VALUES (10, 'wellington', 1);
COMMIT;

-- Dump the data to prove it worked
SELECT * FROM users JOIN addresses on users.id = addresses.user_id;

-- notice that we can't insert just a user or just an address (even with constraints)
-- BEGIN;
-- SET CONSTRAINTS ALL DEFERRED;
-- INSERT INTO users VALUES (2, 'foo2@bar.com', 11);
-- COMMIT;
--
-- BEGIN;
-- SET CONSTRAINTS ALL DEFERRED;
-- INSERT INTO addresses VALUES (11, 'wellington2', 2);
-- COMMIT;

