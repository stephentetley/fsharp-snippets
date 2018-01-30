-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_tsp_vertex_table.sql

-- Note to self: "Vertex = Node" (they are synonyms).

-- DROP TABLE spt_tsp_vertex_table;



CREATE TABLE spt_tsp_vertex_table (
    id INT4 PRIMARY KEY,
    x FLOAT(8) NOT NULL, 
    y FLOAT(8) NOT NULL
);



