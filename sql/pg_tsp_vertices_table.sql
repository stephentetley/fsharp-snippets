-- Must have PostGIS extensions enabled
-- At the prompt:
-- some_db=# \i d:/path_to/sql/pg_tsp_vertices_table.sql

-- Note to self: "Vertex = Node" (they are synonyms).

DROP TABLE spt_tsp_vertices;


-- Should id be generate by the database?, e.g.
-- id SERIAL PRIMARY KEY,
-- or:
-- id INTEGER PRIMARY KEY,

CREATE TABLE spt_tsp_vertices (
    id SERIAL PRIMARY KEY,
    x FLOAT NOT NULL, 
    y FLOAT NOT NULL,
    label VARCHAR(200) NOT NULL
);



