module Scripts.PostGIS

open Npgsql

open SL.AnswerMonad
open SL.ScriptMonad
open SL.Geo
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

let genConvexHullQuery (points:Coord.WGS84Point list) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConvexHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                )) );
    """, (WellKnownText.genMULTIPOINT points) )


let pgConvexHull (points:Coord.WGS84Point list) : PGSQLConn<string> = 
    let query = genConvexHullQuery points
    execReaderSingleton query <| fun reader -> reader.GetString(0)


// Note TargetPercent of 1.0 gives a convex hull (0.9 seems okay)
let genConcaveHullQuery (points:Coord.WGS84Point list) (targetPercent:float) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConcaveHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                ), {1}) );
    """, (WellKnownText.genMULTIPOINT points), targetPercent)


let pgConcaveHull (points:Coord.WGS84Point list) (targetPercent:float) : PGSQLConn<string> = 
    let query = genConcaveHullQuery points targetPercent
    execReaderSingleton query <| fun reader -> reader.GetString(0)