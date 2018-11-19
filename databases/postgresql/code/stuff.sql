create table owners (
  id ,
  name varchar(100),
  email varchar(100),
)

create table dogs (
  id ,
  name varchar(100),
  breed varchar(100),
  owner_id integer,
  age integer,
)

