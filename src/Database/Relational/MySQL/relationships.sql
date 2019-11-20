-- Generate a list of pairs foreign key - primary key.
-- Adapted from: <https://dataedo.com/kb/query/mysql/list-of-foreign-keys-with-columns>.

select concat(fks.constraint_schema, '.', fks.table_name, '.', kcu.column_name) as fk,
       concat(fks.unique_constraint_schema, '.', fks.referenced_table_name, '.', kcu.referenced_column_name) as pk,
from information_schema.referential_constraints fks
join information_schema.key_column_usage kcu
     on fks.constraint_schema = kcu.table_schema
     and fks.table_name = kcu.table_name
     and fks.constraint_name = kcu.constraint_name
where kcu.table_schema not in('information_schema','sys', 'mysql', 'performance_schema')
--    and fks.constraint_schema = 'database name'
;
