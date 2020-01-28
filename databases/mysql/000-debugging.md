
```sql
-- get estimate of num rows of big tables
-- if db name begins with blah
select * from innodb_tablestats where name like 'blah%';


-- show size on disk estimate for all databases
select table_schema, round(sum(data_length+index_length)/1024/1024/1024,2) "size in GB" from information_schema.tables group by 1 order by 2 desc;

-- show size on disk estimates of tables in the blah database
select table_schema, table_name, (data_length/1024) as "data in MB", (index_length/1024) as "indexes in MB", round((data_length + index_length)/1024/1024, 2) as "Total size in GB" from information_schema.tables where table_schema = 'blah';


-- find the largest rows in the wp_options table
select option_name, char_length(option_value)  from wp_options order by 2 desc limit 30;


-- find long option_value values in wp_options
select option_name from wp_options where char_length(option_value) > 100000;
```
