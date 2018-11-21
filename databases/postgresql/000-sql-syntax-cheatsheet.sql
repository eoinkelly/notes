-- Note: None of these expressions include a trailing ; character (to avoid copy
--       & paste errors)

-- --------------------------------------
-- Sequences
-- --------------------------------------

-- Create a sequence
CREATE SEQUENCE parts_id_seq;

-- Full version of creating a sequence (equivalent to the one above)
CREATE SEQUENCE parts_id_seq_2
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;

-- Change owner (default owner will be user which created the sequence)
ALTER TABLE parts_id_seq OWNER TO eoinkelly;

-- Drop the sequence
DROP SEQUENCE parts_id_seq;

-- --------------------------------------
-- Tables
-- --------------------------------------

CREATE TABLE parts (

  -- This is how Rails 5.1+  creates id columns (older Rails used 'integer' not 'bigint')
  -- 1. Create the 'id' column
  id bigint NOT NULL DEFAULT nextval('parts_id_seq'::regclass),
  -- 2. Create a primary key constraint that references it
  CONSTRAINT parts_pkey PRIMARY KEY (id),


  -- this is how Rails creates strings
  -- text, character varying, varchar are all equivalent
  email character varying NOT NULL DEFAULT ''::character varying,

  -- these are all identical
  user_name character varying,
  user_name varchar,
  user_name text, -- text is not in SQL standard but many DBs implement it

  -- integers
  some_int integer,
  bigger_int bigint,
  some_other_int integer NOT NULL DEFAULT 0,

  some_bool boolean,

  -- this is how Rails creates timestamps
  created_at timestamp without time zone NOT NULL,
  updated_at timestamp without time zone NOT NULL,


  -- Create a foreign key
  CONSTRAINT some_foreign_key_name
    FOREIGN KEY (machine_id)
    REFERENCES machines (id) MATCH SIMPLE
      ON UPDATE NO ACTION
      ON DELETE NO ACTION
      DEFERRABLE INITIALLY DEFERRED
)
-- You can omit this. It is included for compatibility with PG 8.0 and earlier.
-- Those old versions enabled OIDs by default for user created tables - that
-- behaviour is strongly discouraged now (OIDs are only useful if you have a
-- table which doesn't have a unique primary key for some reason)
WITH ( OIDS = FALSE)


-- insert three rows in one statement by wrapping each row in parentheses
-- INSERT INTO parts VALUES (a, b, c), (d, e, f), (g, h, i);


--                                                 Table "public.admin_users"
--           Column           |            Type             | Collation | Nullable |                 Default
-- ---------------------------+-----------------------------+-----------+----------+-----------------------------------------
--  id                        | bigint                      |           | not null | nextval('admin_users_id_seq'::regclass)
--  email                     | character varying           |           | not null | ''::character varying
--  encrypted_password        | character varying           |           | not null | ''::character varying
--  reset_password_token      | character varying           |           |          |
--  reset_password_sent_at    | timestamp without time zone |           |          |
--  sign_in_count             | integer                     |           | not null | 0
--  current_sign_in_at        | timestamp without time zone |           |          |
--  last_sign_in_at           | timestamp without time zone |           |          |
--  current_sign_in_ip        | inet                        |           |          |
--  last_sign_in_ip           | inet                        |           |          |
--  created_at                | timestamp without time zone |           | not null |
--  updated_at                | timestamp without time zone |           | not null |
--  user_name                 | character varying           |           |          |
--  encrypted_otp_secret      | character varying           |           |          |
--  encrypted_otp_secret_iv   | character varying           |           |          |
--  encrypted_otp_secret_salt | character varying           |           |          |
--  consumed_timestep         | integer                     |           |          |
--  otp_required_for_login    | boolean                     |           |          |
--  enabled_otp_at            | timestamp without time zone |           |          |
--  failed_attempts           | integer                     |           | not null | 0
--  unlock_token              | character varying           |           |          |
--  locked_at                 | timestamp without time zone |           |          |
--  password_changed_at       | timestamp without time zone |           |          |
--  session_token             | character varying           |           |          |
-- Indexes:
--     "admin_users_pkey" PRIMARY KEY, btree (id)
--     "index_admin_users_on_email" UNIQUE, btree (email)
--     "index_admin_users_on_reset_password_token" UNIQUE, btree (reset_password_token)
--     "index_admin_users_on_unlock_token" UNIQUE, btree (unlock_token)
--     "index_admin_users_on_password_changed_at" btree (password_changed_at)

-- Drop a table
DROP TABLE parts;
