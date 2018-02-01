-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_outfalls_table.sql

DROP TABLE spt_outfalls;


CREATE TABLE spt_outfalls (
    stc25_ref VARCHAR(12) PRIMARY KEY, 
    function_node VARCHAR(30), 
    osgb36_grid VARCHAR(16), 
    point_loc geography (POINT)
);



