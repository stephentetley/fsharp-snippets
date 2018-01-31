module Scripts.PostGIS

open Npgsql

open SL.AnswerMonad
open SL.ScriptMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn


// Common Script type for working with PGSQL connections
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)





// TODO - do we actually want a library of "common" queries?

// TODO - invoke PostGIS, send WKT (and WKB?) in and out... 

// e.g ST_Centroid, ST_AsGeoJSON, ST_GeomFromText
// (pgr_tsp is possible but it seems to require an existing table and more data setup...)

// files with WKT for QGIS can have any number of fields


let singletonWithReader (query:string) (proc:NpgsqlDataReader -> 'a) : Script<'a> = 
    liftWithConnParams << runPGSQLConn <| execReaderSingleton query proc

let singletonAsText1 (query:string) : Script<string> = 
    liftWithConnParams 
        << runPGSQLConn << execReaderSingleton query <| fun reader -> reader.GetString(0)

// ***** Distance Spheroid

// Absolutely must use ST_DistanceSpheroid!
let makeDistanceQUERY (point1:WGS84Point) (point2:WGS84Point) : string = 
    System.String.Format("""
        SELECT ST_DistanceSpheroid(
            ST_GeomFromText('{0}', 4326),
            ST_GeomFromText('{1}', 4326),
            '{2}');
        """, showWktPoint <| wgs84PointToWKT point1
           , showWktPoint <| wgs84PointToWKT point2
           , wgs84Spheroid)


let pgDistanceSpheroid (point1:WGS84Point) (point2:WGS84Point) : Script<float<kilometer>> = 
    let procM (reader:NpgsqlDataReader) : float<kilometer> = 
        0.001<kilometer> * (float <| reader.GetDouble(0))
    singletonWithReader (makeDistanceQUERY point1 point2) procM  


// ***** Concave and covex hulls


let private makeConvexHullQUERY (points:WGS84Point list) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConvexHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                )) );
        """, (genMULTIPOINT points) )

/// Returns WellKnownText.
/// May return different geometry types depending on number of points in the result set.
/// One point - POINT
/// Two points - LINESTRING
/// Three or more points - POLYGON
let pgConvexHull (points:WGS84Point list) : Script<string> = 
    singletonAsText1 <| makeConvexHullQUERY points
    

// Note TargetPercent of 1.0 gives a convex hull (0.9 seems okay)
let private makeConcaveHullQUERY (points:WGS84Point list) (targetPercent:float) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConcaveHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                ), {1}) );
        """, genMULTIPOINT points, targetPercent)

/// Returns WellKnownText.
/// May return different geometry types depending on number of points in the result set.
/// One point - POINT
/// Two points - LINESTRING
/// Three or more points - POLYGON
let pgConcaveHull (points:WGS84Point list) (targetPercent:float) : Script<string> = 
    singletonAsText1 <| makeConcaveHullQUERY points targetPercent


// ***** Centroid

let private makeCentroidQUERY (points:WGS84Point list) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_Centroid('{0}'));
        """, genMULTIPOINT points)

let pgCentroid (points:WGS84Point list) : Script<string> = 
    singletonAsText1 <| makeCentroidQUERY points 
