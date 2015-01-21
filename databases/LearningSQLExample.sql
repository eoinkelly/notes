/* begin table creation */

create table department
 (dept_id smallint unsigned not null auto_increment,
  name varchar(20) not null,
  constraint pk_department primary key (dept_id)
 );

create table branch
 (branch_id smallint unsigned not null auto_increment,
  name varchar(20) not null,
  address varchar(30),
  city varchar(20),
  state varchar(2),
  zip varchar(12),
  constraint pk_branch primary key (branch_id)
 );

create table employee
 (emp_id smallint unsigned not null auto_increment,
  fname varchar(20) not null,
  lname varchar(20) not null,
  start_date date not null,
  end_date date,
  superior_emp_id smallint unsigned,
  dept_id smallint unsigned,
  title varchar(20),
  assigned_branch_id smallint unsigned,
  constraint fk_e_emp_id 
    foreign key (superior_emp_id) references employee (emp_id),
  constraint fk_dept_id
    foreign key (dept_id) references department (dept_id),
  constraint fk_e_branch_id
    foreign key (assigned_branch_id) references branch (branch_id),
  constraint pk_employee primary key (emp_id)
 );

create table product_type
 (product_type_cd varchar(10) not null,
  name varchar(50) not null,
  constraint pk_product_type primary key (product_type_cd)
 );

create table product
 (product_cd varchar(10) not null,
  name varchar(50) not null,
  product_type_cd varchar(10) not null,
  date_offered date,
  date_retired date,
  constraint fk_product_type_cd foreign key (product_type_cd) 
    references product_type (product_type_cd),
  constraint pk_product primary key (product_cd)
 );

create table customer
 (cust_id integer unsigned not null auto_increment,
  fed_id varchar(12) not null,
  cust_type_cd enum('I','B') not null,
  address varchar(30),
  city varchar(20),
  state varchar(20),
  postal_code varchar(10),
  constraint pk_customer primary key (cust_id)
 );

create table individual
 (cust_id integer unsigned not null,
  fname varchar(30) not null,
  lname varchar(30) not null,
  birth_date date,
  constraint fk_i_cust_id foreign key (cust_id)
    references customer (cust_id),
  constraint pk_individual primary key (cust_id)
 );

create table business
 (cust_id integer unsigned not null,
  name varchar(40) not null,
  state_id varchar(10) not null,
  incorp_date date,
  constraint fk_b_cust_id foreign key (cust_id)
    references customer (cust_id),
  constraint pk_business primary key (cust_id)
 );

create table officer
 (officer_id smallint unsigned not null auto_increment,
  cust_id integer unsigned not null,
  fname varchar(30) not null,
  lname varchar(30) not null,
  title varchar(20),
  start_date date not null,
  end_date date,
  constraint fk_o_cust_id foreign key (cust_id)
    references business (cust_id),
  constraint pk_officer primary key (officer_id)
 );

create table account
 (account_id integer unsigned not null auto_increment,
  product_cd varchar(10) not null,
  cust_id integer unsigned not null,
  open_date date not null,
  close_date date,
  last_activity_date date,
  status enum('ACTIVE','CLOSED','FROZEN'),
  open_branch_id smallint unsigned,
  open_emp_id smallint unsigned,
  avail_balance float(10,2),
  pending_balance float(10,2),
  constraint fk_product_cd foreign key (product_cd)
    references product (product_cd),
  constraint fk_a_cust_id foreign key (cust_id)
    references customer (cust_id),
  constraint fk_a_branch_id foreign key (open_branch_id)
    references branch (branch_id),
  constraint fk_a_emp_id foreign key (open_emp_id)
    references employee (emp_id),
  constraint pk_account primary key (account_id)
 );

create table transaction
 (txn_id integer unsigned not null auto_increment,
  txn_date datetime not null,
  account_id integer unsigned not null,
  txn_type_cd enum('DBT','CDT'),
  amount double(10,2) not null,
  teller_emp_id smallint unsigned,
  execution_branch_id smallint unsigned,
  funds_avail_date datetime,
  constraint fk_t_account_id foreign key (account_id)
    references account (account_id),
  constraint fk_teller_emp_id foreign key (teller_emp_id)
    references employee (emp_id),
  constraint fk_exec_branch_id foreign key (execution_branch_id)
    references branch (branch_id),
  constraint pk_transaction primary key (txn_id)
 );

/* end table creation */

/* begin data population */

/* department data */
insert into department 
 (dept_id, name)
values 
(null, 'Operations'),
(null, 'Loans'),
(null, 'Administration');

/* branch data */
insert into branch 
 (branch_id, name, address, city, state, zip)
values 
(null, 'Headquarters', '3882 Main St.', 'Waltham', 'MA', '02451'),
(null, 'Woburn Branch', '422 Maple St.', 'Woburn', 'MA', '01801'),
(null, 'Quincy Branch', '125 Presidential Way', 'Quincy', 'MA', '02169'),
(null, 'So. NH Branch', '378 Maynard Ln.', 'Salem', 'NH', '03079');

/* employee data */
insert into employee 
 (emp_id, fname, lname, start_date, 
  dept_id, title, assigned_branch_id)
values 
(null, 'Michael', 'Smith', '2005-06-22', 
  (select dept_id from department where name = 'Administration'), 
  'President', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Susan', 'Barker', '2006-09-12', 
  (select dept_id from department where name = 'Administration'), 
  'Vice President', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Robert', 'Tyler', '2005-02-09',
  (select dept_id from department where name = 'Administration'), 
  'Treasurer', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Susan', 'Hawthorne', '2006-04-24', 
  (select dept_id from department where name = 'Operations'), 
  'Operations Manager', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'John', 'Gooding', '2007-11-14', 
  (select dept_id from department where name = 'Loans'), 
  'Loan Manager', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Helen', 'Fleming', '2008-03-17', 
  (select dept_id from department where name = 'Operations'), 
  'Head Teller', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Chris', 'Tucker', '2008-09-15', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Sarah', 'Parker', '2006-12-02', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Jane', 'Grossman', '2006-05-03', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Headquarters')),
(null, 'Paula', 'Roberts', '2006-07-27', 
  (select dept_id from department where name = 'Operations'), 
  'Head Teller', 
  (select branch_id from branch where name = 'Woburn Branch')),
(null, 'Thomas', 'Ziegler', '2004-10-23', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Woburn Branch')),
(null, 'Samantha', 'Jameson', '2007-01-08', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Woburn Branch')),
(null, 'John', 'Blake', '2004-05-11', 
  (select dept_id from department where name = 'Operations'), 
  'Head Teller', 
  (select branch_id from branch where name = 'Quincy Branch')),
(null, 'Cindy', 'Mason', '2006-08-09', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Quincy Branch')),
(null, 'Frank', 'Portman', '2007-04-01', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'Quincy Branch')),
(null, 'Theresa', 'Markham', '2005-03-15', 
  (select dept_id from department where name = 'Operations'), 
  'Head Teller', 
  (select branch_id from branch where name = 'So. NH Branch')),
(null, 'Beth', 'Fowler', '2006-06-29', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'So. NH Branch')),
(null, 'Rick', 'Tulman', '2006-12-12', 
  (select dept_id from department where name = 'Operations'), 
  'Teller', 
  (select branch_id from branch where name = 'So. NH Branch'));

/* create data for self-referencing foreign key 'superior_emp_id' */
create temporary table emp_tmp as
select emp_id, fname, lname from employee;

update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Smith' and fname = 'Michael')
where ((lname = 'Barker' and fname = 'Susan')
  or (lname = 'Tyler' and fname = 'Robert'));
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Tyler' and fname = 'Robert')
where lname = 'Hawthorne' and fname = 'Susan';
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Hawthorne' and fname = 'Susan')
where ((lname = 'Gooding' and fname = 'John')
  or (lname = 'Fleming' and fname = 'Helen')
  or (lname = 'Roberts' and fname = 'Paula') 
  or (lname = 'Blake' and fname = 'John') 
  or (lname = 'Markham' and fname = 'Theresa')); 
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Fleming' and fname = 'Helen')
where ((lname = 'Tucker' and fname = 'Chris') 
  or (lname = 'Parker' and fname = 'Sarah') 
  or (lname = 'Grossman' and fname = 'Jane'));  
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Roberts' and fname = 'Paula')
where ((lname = 'Ziegler' and fname = 'Thomas')  
  or (lname = 'Jameson' and fname = 'Samantha'));   
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Blake' and fname = 'John')
where ((lname = 'Mason' and fname = 'Cindy')   
  or (lname = 'Portman' and fname = 'Frank'));    
update employee set superior_emp_id =
 (select emp_id from emp_tmp where lname = 'Markham' and fname = 'Theresa')
where ((lname = 'Fowler' and fname = 'Beth')   
  or (lname = 'Tulman' and fname = 'Rick'));    

drop table emp_tmp;

/* recreate employee self-referencing foreign key */
alter table employee add constraint fk_e_emp_id
foreign key (superior_emp_id) references employee (emp_id);

/* product type data */
insert into product_type 
 (product_type_cd, name)
values 
('ACCOUNT','Customer Accounts'),
('LOAN','Individual and Business Loans'),
('INSURANCE','Insurance Offerings');

/* product data */
insert into product 
 (product_cd, name, product_type_cd, date_offered)
values 
('CHK','checking account','ACCOUNT','2004-01-01'),
('SAV','savings account','ACCOUNT','2004-01-01'),
('MM','money market account','ACCOUNT','2004-01-01'),
('CD','certificate of deposit','ACCOUNT','2004-01-01'),
('MRT','home mortgage','LOAN','2004-01-01'),
('AUT','auto loan','LOAN','2004-01-01'),
('BUS','business line of credit','LOAN','2004-01-01'),
('SBL','small business loan','LOAN','2004-01-01');

/* residential customer data */
insert into customer 
 (cust_id, fed_id, cust_type_cd,
  address, city, state, postal_code)
values 
(null, '111-11-1111', 'I', '47 Mockingbird Ln', 'Lynnfield', 'MA', '01940'),
(null, '222-22-2222', 'I', '372 Clearwater Blvd', 'Woburn', 'MA', '01801'),
(null, '333-33-3333', 'I', '18 Jessup Rd', 'Quincy', 'MA', '02169'),
(null, '444-44-4444', 'I', '12 Buchanan Ln', 'Waltham', 'MA', '02451'),
(null, '555-55-5555', 'I', '2341 Main St', 'Salem', 'NH', '03079'),
(null, '666-66-6666', 'I', '12 Blaylock Ln', 'Waltham', 'MA', '02451'),
(null, '777-77-7777', 'I', '29 Admiral Ln', 'Wilmington', 'MA', '01887'),
(null, '888-88-8888', 'I', '472 Freedom Rd', 'Salem', 'NH', '03079'),
(null, '999-99-9999', 'I', '29 Maple St', 'Newton', 'MA', '02458'),
(null, '04-1111111', 'B', '7 Industrial Way', 'Salem', 'NH', '03079'),
(null, '04-2222222', 'B', '287A Corporate Ave', 'Wilmington', 'MA', '01887'),
(null, '04-3333333', 'B', '789 Main St', 'Salem', 'NH', '03079'),
(null, '04-4444444', 'B', '4772 Presidential Way', 'Quincy', 'MA', '02169');

insert into individual 
 (cust_id, fname, lname, birth_date)
select cust_id, 'James', 'Hadley', '1977-04-22' 
from customer
where fed_id = '111-11-1111'
union all
select cust_id, 'Susan', 'Tingley', '1973-08-15' 
from customer
where fed_id = '222-22-2222'
union all
select cust_id, 'Frank', 'Tucker', '1963-02-06' 
from customer
where fed_id = '333-33-3333'
union all
select cust_id, 'John', 'Hayward', '1971-12-22' 
from customer
where fed_id = '444-44-4444'
union all
select cust_id, 'Charles', 'Frasier', '1976-08-25' 
from customer
where fed_id = '555-55-5555'
union all
select cust_id, 'John', 'Spencer', '1967-09-14' 
from customer
where fed_id = '666-66-6666'
union all
select cust_id, 'Margaret', 'Young', '1951-03-19' 
from customer
where fed_id = '777-77-7777'
union all
select cust_id, 'George', 'Blake', '1982-07-01' 
from customer
where fed_id = '888-88-8888'
union all
select cust_id, 'Richard', 'Farley', '1973-06-16' 
from customer
where fed_id = '999-99-9999';

/* corporate customer data */
insert into business 
 (cust_id, name, state_id, incorp_date)
select cust_id, 'Chilton Engineering', '12-345-678', '1995-05-01' 
from customer
where fed_id = '04-1111111'
union all
select cust_id, 'Northeast Cooling Inc.', '23-456-789', '2001-01-01' 
from customer
where fed_id = '04-2222222'
union all
select cust_id, 'Superior Auto Body', '34-567-890', '2002-06-30' 
from customer
where fed_id = '04-3333333'
union all
select cust_id, 'AAA Insurance Inc.', '45-678-901', '1999-05-01' from customer
where fed_id = '04-4444444';


insert into officer 
 (officer_id, cust_id, fname, lname, title, start_date)
select null, cust_id, 'John', 'Chilton', 'President', '1995-05-01'
from customer
where fed_id = '04-1111111'
union all
select null, cust_id, 'Paul', 'Hardy', 'President', '2001-01-01'
from customer
where fed_id = '04-2222222'
union all
select null, cust_id, 'Carl', 'Lutz', 'President', '2002-06-30'
from customer
where fed_id = '04-3333333'
union all
select null, cust_id, 'Stanley', 'Cheswick', 'President', '1999-05-01'
from customer
where fed_id = '04-4444444';

/* residential account data */
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Woburn' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2000-01-15' open_date, '2005-01-04' last_date,
    1057.75 avail, 1057.75 pend union all
  select 'SAV' prod_cd, '2000-01-15' open_date, '2004-12-19' last_date,
    500.00 avail, 500.00 pend union all
  select 'CD' prod_cd, '2004-06-30' open_date, '2004-06-30' last_date,
    3000.00 avail, 3000.00 pend) a
where c.fed_id = '111-11-1111';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Woburn' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2001-03-12' open_date, '2004-12-27' last_date,
    2258.02 avail, 2258.02 pend union all
  select 'SAV' prod_cd, '2001-03-12' open_date, '2004-12-11' last_date,
    200.00 avail, 200.00 pend) a
where c.fed_id = '222-22-2222';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Quincy' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2002-11-23' open_date, '2004-11-30' last_date,
    1057.75 avail, 1057.75 pend union all
  select 'MM' prod_cd, '2002-12-15' open_date, '2004-12-05' last_date,
    2212.50 avail, 2212.50 pend) a
where c.fed_id = '333-33-3333';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Waltham' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2003-09-12' open_date, '2005-01-03' last_date,
    534.12 avail, 534.12 pend union all
  select 'SAV' prod_cd, '2000-01-15' open_date, '2004-10-24' last_date,
    767.77 avail, 767.77 pend union all
  select 'MM' prod_cd, '2004-09-30' open_date, '2004-11-11' last_date,
    5487.09 avail, 5487.09 pend) a
where c.fed_id = '444-44-4444';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Salem' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2004-01-27' open_date, '2005-01-05' last_date,
    2237.97 avail, 2897.97 pend) a
where c.fed_id = '555-55-5555';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Waltham' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2002-08-24' open_date, '2004-11-29' last_date,
    122.37 avail, 122.37 pend union all
  select 'CD' prod_cd, '2004-12-28' open_date, '2004-12-28' last_date,
    10000.00 avail, 10000.00 pend) a
where c.fed_id = '666-66-6666';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Woburn' limit 1) e
  cross join
 (select 'CD' prod_cd, '2004-01-12' open_date, '2004-01-12' last_date,
    5000.00 avail, 5000.00 pend) a
where c.fed_id = '777-77-7777';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Salem' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2001-05-23' open_date, '2005-01-03' last_date,
    3487.19 avail, 3487.19 pend union all
  select 'SAV' prod_cd, '2001-05-23' open_date, '2004-10-12' last_date,
    387.99 avail, 387.99 pend) a
where c.fed_id = '888-88-8888';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Waltham' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2003-07-30' open_date, '2004-12-15' last_date,
    125.67 avail, 125.67 pend union all
  select 'MM' prod_cd, '2004-10-28' open_date, '2004-10-28' last_date,
    9345.55 avail, 9845.55 pend union all
  select 'CD' prod_cd, '2004-06-30' open_date, '2004-06-30' last_date,
    1500.00 avail, 1500.00 pend) a
where c.fed_id = '999-99-9999';

/* corporate account data */
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Salem' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2002-09-30' open_date, '2004-12-15' last_date,
    23575.12 avail, 23575.12 pend union all
  select 'BUS' prod_cd, '2002-10-01' open_date, '2004-08-28' last_date,
    0 avail, 0 pend) a
where c.fed_id = '04-1111111';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Woburn' limit 1) e
  cross join
 (select 'BUS' prod_cd, '2004-03-22' open_date, '2004-11-14' last_date,
    9345.55 avail, 9345.55 pend) a
where c.fed_id = '04-2222222';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Salem' limit 1) e
  cross join
 (select 'CHK' prod_cd, '2003-07-30' open_date, '2004-12-15' last_date,
    38552.05 avail, 38552.05 pend) a
where c.fed_id = '04-3333333';
insert into account (account_id, product_cd, cust_id, open_date,
  last_activity_date, status, open_branch_id,
  open_emp_id, avail_balance, pending_balance)
select null, a.prod_cd, c.cust_id, a.open_date, a.last_date, 'ACTIVE',
  e.branch_id, e.emp_id, a.avail, a.pend
from customer c cross join 
 (select b.branch_id, e.emp_id 
  from branch b inner join employee e on e.assigned_branch_id = b.branch_id
  where b.city = 'Quincy' limit 1) e
  cross join
 (select 'SBL' prod_cd, '2004-02-22' open_date, '2004-12-17' last_date,
    50000.00 avail, 50000.00 pend) a
where c.fed_id = '04-4444444';

/* put $100 in all checking/savings accounts on Jan 5th, 2008 */
insert into transaction (txn_id, txn_date, account_id, txn_type_cd,
  amount, funds_avail_date)
select null, '2008-01-05', a.account_id, 'DBT', 100, '2008-01-05'
from account a
where a.product_cd IN ('CHK','SAV','CD','MM');

/* end data population */