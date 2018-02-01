-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_hospitals_table.sql

-- DROP TABLE spt_dwithin;


-- This is a sample table for running ST_DWithin queries
-- We need geom, all other fields are for printing  and identification.

CREATE TABLE spt_dwithin (
    uid VARCHAR(16) PRIMARY KEY,
    name VARCHAR(160), 
    function_type VARCHAR(30), 
    osgb36_ref VARCHAR(20), 
    location geography (POINT)
);



