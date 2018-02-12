-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_pathfind_table.sql

DROP TABLE spt_pathfind;


-- Use PostgreSQL's SERIAL type for an auto-incrementing id

CREATE TABLE spt_pathfind (
    id SERIAL PRIMARY KEY,
    basetype VARCHAR(30), 
    function_node VARCHAR(30), 
    start_point geography (POINT), 
    end_point geography (POINT),
    distance_meters float8
);

DEALLOCATE insert_spt_pathfind;

-- Ideally the client code would use this, but invoking it through Npgsql
-- throws an error I can't resolve.
PREPARE insert_spt_pathfind (text,text,text,text) AS
INSERT INTO 
    spt_pathfind (basetype,function_node,start_point,end_point,distance_meters) 
VALUES
    ($1,$2,
     ST_GeogFromText($3),
     ST_GeogFromText($4),
     ST_Distance(ST_GeogFromText($3), ST_GeogFromText($4)) ) ;