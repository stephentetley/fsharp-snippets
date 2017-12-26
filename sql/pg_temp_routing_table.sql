-- For PostgreSQL with PostGIS and pgrouting extensions.
--
-- This is a table for running pgrouting (pgr_tsp) queries.
-- It is expected that data is dropped and repopulated for each 
-- session.


-- DROP TABLE temp_routing;

-- PostGIS / pgrouting (pgr_tsp) seems to like (or need) a numeric
-- id on the coordinate table.
CREATE TABLE temp_routing (
    id integer NOT NULL,
    point_code character varying(30) NOT NULL,
    point_name character varying(150) NOT NULL,
    wgs84lat double precision,
    wgs84lon double precision
);

ALTER TABLE ONLY temp_routing
    ADD CONSTRAINT pk_temp_routing_id PRIMARY KEY (id);