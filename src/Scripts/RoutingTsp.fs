module Scripts.RoutingTsp

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad

open Scripts.PostGIS


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_tsp_vertex_table"


/// wgs84:Longitude is x, wgs84:Latitude is y
let private makeVertexINSERT (serialNum:int) (vertex:WGS84Point) : string = 
    sqlINSERT "spt_tsp_vertex_table" 
            <|  [ intValue      "id"        serialNum
                ; floatValue    "x"         <| float vertex.Longitude
                ; floatValue    "y"         <| float vertex.Latitude
                ]

type VertexInsertDict<'row> = 
    { tryMakeVertexPoint : 'row -> WGS84Point option }


/// Design "problem" 
/// This procedure associates a row with an Id but the Id is (probably) unknown
/// to the user when it is returned from pgr_tsp...
let insertVertices (dict:VertexInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    let proc1 (ix:int) (point:WGS84Point) : PGSQLConn<int> = 
        execNonQuery <| makeVertexINSERT ix point
    let goodData = Seq.choose (dict.tryMakeVertexPoint) vertices 
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseiM proc1 goodData