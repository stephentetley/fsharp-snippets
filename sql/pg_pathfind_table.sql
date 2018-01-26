-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_pathfind_table.sql

DROP TABLE spt_pathfind;


-- Use PostgreSQL's SERIAL typer for an auto-incrementing id

CREATE TABLE spt_pathfind (
    id SERIAL PRIMARY KEY,
    basetype VARCHAR(30), 
    function_node VARCHAR(30), 
    start_point geography (POINT), 
    end_point geography (POINT)
);



