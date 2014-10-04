-- Table: parts

-- DROP TABLE parts;

-- to fix: make the sequence

CREATE TABLE machines (
  id          INTEGER NOT NULL DEFAULT nextval('machines_id_seq'::regclass),
  description TEXT
)

CREATE TABLE parts
(
  id integer NOT NULL DEFAULT nextval('parts_id_seq'::regclass),
  version integer,
  title character varying(510) DEFAULT NULL::character varying,
  description text,
  size integer,
  created_at timestamp with time zone,
  updated_at timestamp with time zone,
  private boolean,


  machine_id integer,

  -- Create a primary key
  CONSTRAINT parts_pkey PRIMARY KEY (id),

  -- Create a foreign key
  CONSTRAINT parts_ibfk_1 FOREIGN KEY (machine_id)
      REFERENCES machines (id) MATCH SIMPLE
      ON UPDATE NO ACTION
      ON DELETE NO ACTION
      DEFERRABLE INITIALLY DEFERRED
)
-- ???
WITH (
  OIDS=FALSE
);


-- ------------------------------------------------
-- UNION and coercion example

DROP TABLE t1;

DROP TABLE t2;

CREATE TABLE t1 (
  x integer,
  y float
)

CREATE TABLE t2 (
  x float
  y integer,
)

INSERT INTO t1 (0, 1.0)
INSERT INTO t1 (0, 2.0)

INSERT INTO t2 (0.0, 0)
INSERT INTO t2 (0.0, 1)
INSERT INTO t2 (1.0, 2)

SELECT x,y FROM t1 UNION SELECT x,y FROM t2;
