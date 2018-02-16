module Scripts.PostGIS

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.ScriptMonad

// Common Script type for working with PGSQL connections
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)



let private singletonWithReader (query:string) (proc:NpgsqlDataReader -> 'a) : Script<'a> = 
    liftWithConnParams << runPGSQLConn <| execReaderSingleton query proc

let private singletonAsText1 (query:string) : Script<string> = 
    singletonWithReader query <| fun reader -> reader.GetString(0)


// TODO - how much of the PostGIS API can we wrap? (do we want to?) 
// e.g ST_Centroid, ST_AsGeoJSON, ST_GeomFromText

// pgr_tsp can't really be wrapped. It requires an existing table and data setup, 
// thus we consider it a script in itself.

// TODO - be more clever about the input and output arguments.
// Should be know if we are sending / receiving Geom, WKT (and WKB?) etc.

// In this module we shouldn't consider generating output for 
// e.g. QGIS Delimited Text imports. This is the domain of scripts. 
// Note though, for QGIS files with WKT can have any number of fields.


/// Generates ```ST_GeogFromText('SRID=4326;POINT(1.12345 2.12345')```
/// The name for this needs to be considered.
/// Favour calling ST_Point instead.
let makeSTGeogFromTextPointLiteral (pt:WGS84Point) : string = 
    sprintf "ST_GeogFromText('SRID=4326;%s')" << showWktPoint <| wgs84WktPoint pt



// ***** Distance Spheroid

// Absolutely must use ST_DistanceSpheroid!
let makeDistanceQUERY (point1:WGS84Point) (point2:WGS84Point) : string = 
    System.String.Format("""
        SELECT ST_DistanceSpheroid(
            ST_GeomFromText('{0}', 4326),
            ST_GeomFromText('{1}', 4326),
            '{2}');
        """, showWktPoint <| wgs84WktPoint point1
           , showWktPoint <| wgs84WktPoint point2
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
        """, showWktMultiPoint << WktMultiPoint <| wgs84WktCoordList points )

/// Returns WellKnownText.
/// May return different geometry types depending on number of points in the result set.
/// One point - POINT
/// Two points - LINESTRING
/// Three or more points - POLYGON
let pgConvexHull (points:WGS84Point list) : Script<WellKnownText<WGS84>> = 
    fmapM WellKnownText << singletonAsText1 <| makeConvexHullQUERY points
    

// Note TargetPercent of 1.0 gives a convex hull (0.9 seems okay)
let private makeConcaveHullQUERY (points:WGS84Point list) (targetPercent:float) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConcaveHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                ), {1}) );
        """, showWktMultiPoint << WktMultiPoint <| wgs84WktCoordList points
           , targetPercent)

/// Returns WellKnownText.
/// May return different geometry types depending on number of points in the result set.
/// One point - POINT
/// Two points - LINESTRING
/// Three or more points - POLYGON
///
/// TODO - need to return a composite geometry type rather then WellKnownText<WGS84>
let pgConcaveHull (points:WGS84Point list) (targetPercent:float) : Script<WellKnownText<WGS84>> = 
    fmapM WellKnownText << singletonAsText1 <| makeConcaveHullQUERY points targetPercent


// ***** Centroid

let private makeCentroidQUERY (dict:WktCoordIso<'point,'srid>) (points:seq<'point>) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_Centroid('{0}'));
        """, showWktMultiPoint <| makeWktMultiPoint dict points)


// TODO - pgCentroid should obviously return a point...
// Which in reality probably needs to be ``point option``.
// Ideally we shouldn't be fixed to WGS84Points.

let pgCentroidOld (points:WGS84Point list) : Script<string> = 
    singletonAsText1 <| makeCentroidQUERY wktIsoWGS84 points 

/// TODO - generalizing...
/// The input should not need to be a point list it should be a 
/// geometry like a polygon or a multipoint.
/// (or even a list of geometries?)
let pgCentroid (dict:WktCoordIso<'point,'srid>) (points:seq<'point>) : Script<'point option> = 
    scriptMonad {
        let! wkt = singletonAsText1 <| makeCentroidQUERY dict points 
        let optPoint = Option.bind (wktExtractPoint dict) <| tryReadWktPoint wkt
        return optPoint
        }

