begin;

drop table if exists owners;

create table owners (
  id bigint primary key,
  name text,
  email text
);

insert into owners VALUES
  (1, 'john', 'john@example.com'),
  (2, 'jane', 'jane@example.com'),
  (3, 'bill', 'bill@example.com'),
  (4, 'brad', 'brad@example.com'),
  (5, 'mike', 'mike@example.com');

drop table if exists dogs;

create table dogs (
  id bigint primary key,
  name text,
  owner_id bigint
);

insert into dogs VALUES
  (1, 'shep', 1),
  (2, 'brandy', 1),
  (3, 'fido', 2),
  (4, 'scooby', 2),
  (5, 'mitzy', 3),
  (6, 'barky', null)
  ;

commit;


-- Use a real LEFT OUTER JOIN from the database
select * from owners left outer join dogs on dogs.owner_id = owners.id;

-- ************************************
-- Make a LEFT OUTER JOIN from scratch
--
-- The ordering between my "from scratch" one and the real one is different
--
-- ************************************
with
  inner_join_all_fields as (
    --  id | name | email | id |  name  | owner_id
    select * from owners join dogs on dogs.owner_id = owners.id
  ),
  inner_join_just_owners as (
    --  id | name | email
    select owners.* from owners join dogs on dogs.owner_id = owners.id
  ),
  owner_leftovers as (
    --  id | name | email
    select owners.* from owners
    except
    select * from inner_join_just_owners
  ),
  owner_leftovers_extended_w_nulls as (
    --  id | name | email | id |  name  | owner_id
    select *,
      null::bigint as id,
      null::text as name,
      null::bigint as owner_id
     from owner_leftovers
  ),
  left_outer_join as (
    --  id | name | email | id |  name  | owner_id
    select * from owner_leftovers_extended_w_nulls
    union
    select * from inner_join_all_fields
  )
select * from left_outer_join ;
