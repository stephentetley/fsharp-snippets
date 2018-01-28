-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_hospitals_table.sql

-- DROP TABLE spt_hospitals;


-- Note 'PRIMARY KEY' automatically makes the the column NOT NULL in PostgreSQL.

CREATE TABLE spt_hospitals (
    name VARCHAR(100) PRIMARY KEY,
    telephone VARCHAR(30), 
    address VARCHAR(300), 
    postcode VARCHAR(20), 
    grid_ref geography (POINT)
);



