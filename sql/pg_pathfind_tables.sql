-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_pathfind_table.sql

DROP TABLE spt_pathfind_edges;
DROP TABLE spt_pathfind_nodes;

-- Use PostgreSQL's SERIAL type for an auto-incrementing id

-- Spatial reference is WGS84 (SRID:4326)

CREATE TABLE spt_pathfind_edges (
    id SERIAL PRIMARY KEY,
    type_tag VARCHAR(30), 
    edge_label VARCHAR(60), 
    start_point geography (POINT,4326), 
    end_point geography (POINT,4326),
    distance_meters float8
);


CREATE TABLE spt_pathfind_nodes (
    id SERIAL PRIMARY KEY,
    type_tag VARCHAR(30), 
    node_label VARCHAR(60), 
    grid_ref geography (POINT,4326)
);


-- Note 
-- Ideally the client code would use the prepared statement below, 
-- but invoking it through Npgsql throws an error I can't resolve.


DEALLOCATE insert_spt_pathfind_edge;

PREPARE insert_spt_pathfind_edge (text,text,text,text) AS
INSERT INTO 
    spt_pathfind_edges (type_tag,edge_label,start_point,end_point,distance_meters) 
VALUES
    ($1,$2,
     ST_GeogFromText($3),
     ST_GeogFromText($4),
     ST_Distance(ST_GeogFromText($3), ST_GeogFromText($4)) ) ;