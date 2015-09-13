
-- QUESTION: are postgres types uppercase?

DROP TABLE machines;
DROP SEQUENCE parts_id_seq;
DROP TABLE parts;

-- support cast of tables required for foreign keys on the star!
create table machines (id integer NOT NULL);

CREATE SEQUENCE parts_id_seq
  INCREMENT 1
  MINVALUE 1
  MAXVALUE 9223372036854775807
  START 1
  CACHE 1;
-- ALTER TABLE parts_id_seq OWNER TO eoinkelly;
-- DROP SEQUENCE parts_id_seq;


CREATE TABLE parts (
  unique_key integer NOT NULL DEFAULT nextval('parts_id_seq'::regclass),
  some_nullable_varchar character varying(510) DEFAULT NULL::character varying,
  some_text text,
  some_int integer,
  some_bool boolean,
  some_timestamp timestamp with time zone,

  -- Create a primary key
  CONSTRAINT some_parts_index_name PRIMARY KEY (id),

  -- Create a foreign key
  CONSTRAINT some_foreign_key_name FOREIGN KEY (machine_id)
      REFERENCES machines (id) MATCH SIMPLE
      ON UPDATE NO ACTION
      ON DELETE NO ACTION
      DEFERRABLE INITIALLY DEFERRED
)
-- ???
WITH (
  OIDS=FALSE
);
)
