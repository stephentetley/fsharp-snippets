module Scripts.RoutingTsp

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad

open Scripts.PostGIS


// API note - two views of pgr_tsp. Either it:
// [a] Generates a route of List<label * grid_ref * cost> 
// [b] Or it is a sorting algorithm.
// Of course we will want to print the output (to Csv etc.) but the API
// should let us sort some arbitrary typed input into shortest route order.
//
// Note - some filtering will occur if the data is bad 
// (missing or invalid grid refs).




let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRows "spt_tsp_vertices"


/// wgs84:Longitude is x, wgs84:Latitude is y
/// We have a label in the vertex table so we can interpret the data after the route 
/// is generated.
let private makeVertexINSERT (serialNum:int) (vertex:WGS84Point) (label:string) : string = 
    sqlINSERT "spt_tsp_vertices" 
            <|  [ intValue      "id"        serialNum
                ; floatValue    "x"         <| float vertex.Longitude
                ; floatValue    "y"         <| float vertex.Latitude
                ; stringValue   "label"     label
                ]

type VertexInsertDict<'row> = 
    { TryMakeVertexPoint : 'row -> WGS84Point option
      MakeVertexLabel: 'row -> string }


/// Design "problem" 
/// This procedure associates a row with an Id but the Id is (probably) unknown
/// to the user when it is returned from pgr_tsp, hence I think we need label 
/// and a join.
let insertVertices (dict:VertexInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    let proc1 (ix:int) (point:WGS84Point, label:string) : PGSQLConn<int> = 
        execNonQuery <| makeVertexINSERT ix point label
    
    let good1 (row:'row) : (WGS84Point * string) option = 
         Option.map (fun pt -> (pt, dict.MakeVertexLabel row)) 
            <| dict.TryMakeVertexPoint row

    let goodData = Seq.choose id <| Seq.map good1 vertices 

    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseiM proc1 goodData

let SetupVertexDB (dict:VertexInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    scriptMonad { 
        let! _      = deleteAllData ()              |> logScript (sprintf "%i rows deleted")
        let! count  = insertVertices dict vertices  |> logScript (sprintf "%i rows inserted") 
        return count
     }


let makeEucledianTSPQUERY (startId:int) (endId:int) : string = 
    System.String.Format("""
        SELECT r.seq, r.node, v.label, v.x, v.y, r.cost, r.agg_cost
        FROM 
            pgr_eucledianTSP( 'SELECT id,x,y FROM spt_tsp_vertices', 
                    start_id := {0},
                    end_id := {1}
                   ) AS r
        LEFT JOIN spt_tsp_vertices AS v ON r.node = v.id;
    """, startId, endId)

/// NodeLabel must be a short string so we can store it in the DB.
type RouteNode = 
    { SeqNumber: int
      NodeLabel: string
      GridRef: WGS84Point
      Cost: float
      AggCost: float }

type Route = RouteNode list

let eucledianTspQuery (startId:int) (endId:int) : Script<Route> = 
    let query = makeEucledianTSPQUERY startId endId
    let procM (reader:NpgsqlDataReader) : RouteNode = 
        let gridRef = 
            let lon = float <| reader.GetDouble(3)
            let lat = float <| reader.GetDouble(4)
            { Longitude = 1.0<degree> * lon; Latitude = 1.0<degree> * lat}
        { SeqNumber          = reader.GetInt32(0)
        ; NodeLabel     = reader.GetString(2)
        ; GridRef       = gridRef
        ; Cost          = float <| reader.GetDouble(5)
        ; AggCost       = float<| reader.GetDouble(6) }  // TODO null for first...
    liftWithConnParams << runPGSQLConn <| execReaderList query procM  
