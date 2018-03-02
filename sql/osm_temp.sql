-- SELECT * from planet_osm_point WHERE railway = 'station' LIMIT 5;

-- SELECT osm_id, name, railway from planet_osm_point WHERE railway = 'station' LIMIT 5;

SELECT * FROM planet_osm_point WHERE railway = 'station' AND name= 'Saltaire';

SELECT osm_id, name, railway, way FROM planet_osm_point WHERE railway = 'station' AND name='Saltaire';


-- Saltaire Staion, depending where measured from the location i Lat: 53.838505, Lon: -1.7904701

SELECT osm_id, name, railway, ST_AsText(ST_Transform(way,4326)) as loc FROM planet_osm_point WHERE railway = 'station' AND name='Saltaire';

SELECT osm_id, name, railway, ST_AsText(ST_Transform(way,4326)) as loc FROM planet_osm_point WHERE railway = 'station' ORDER BY name;
