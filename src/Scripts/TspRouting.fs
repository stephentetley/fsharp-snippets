module Scripts.TspRouting

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.Geo.WellKnownText
open SL.ScriptMonad

open Scripts.PostGIS


// Generate a route of List<label * grid_ref * cost> 

// We want to:
// (1) print out to Csv in order, 
// (2) generate a WKT linestring for viewing
// Maybe other things...




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

type TspVertexInsertDict<'row> = 
    { TryMakeVertexPoint : 'row -> WGS84Point option
      MakeVertexLabel: 'row -> string }


/// Design "problem" 
/// This procedure associates a row with an Id but the Id is (probably) unknown
/// to the user when it is returned from pgr_tsp, hence I think we need label 
/// and a join.
let insertVertices (dict:TspVertexInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    let proc1 (ix:int) (point:WGS84Point, label:string) : PGSQLConn<int> = 
        execNonQuery <| makeVertexINSERT ix point label
    
    let good1 (row:'row) : (WGS84Point * string) option = 
         Option.map (fun pt -> (pt, dict.MakeVertexLabel row)) 
            <| dict.TryMakeVertexPoint row

    let goodData = Seq.choose id <| Seq.map good1 vertices 

    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseiM proc1 goodData

let SetupTspVertexDB (dict:TspVertexInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
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

let private dropLast (source:'a list) : 'a list = 
    let rec work ac xs = 
        match xs with
        | [] -> List.rev ac
        | [x] -> List.rev ac
        | x :: xs -> work (x::ac) xs
    work [] source

let eucledianTSP (startId:int) (endId:int) : Script<Route> = 
    let query = makeEucledianTSPQUERY startId endId
    let procM (reader:NpgsqlDataReader) : RouteNode = 
        let gridRef = 
            let lon = float <| reader.GetDouble(3)
            let lat = float <| reader.GetDouble(4)
            { Longitude = 1.0<degree> * lon; Latitude = 1.0<degree> * lat}
        { SeqNumber     = reader.GetInt32(0)
        ; NodeLabel     = reader.GetString(2)
        ; GridRef       = gridRef
        ; Cost          = float <| reader.GetDouble(5)
        ; AggCost       = float<| reader.GetDouble(6) } 
    fmapM dropLast << liftWithConnParams << runPGSQLConn <| execReaderList query procM  


let makeFindIdByLabelQUERY (label:string) : string = 
    System.String.Format("""
        SELECT id FROM spt_tsp_vertices WHERE label='{0}';
        """, label)

let private findIdQuery (query:string) : Script<int> = 
    let procM (reader:NpgsqlDataReader) : int = reader.GetInt32(0)
    liftWithConnParams << runPGSQLConn <| execReaderFirst query procM  


let findIdByLabel (label:string) : Script<int> = 
    findIdQuery <| makeFindIdByLabelQUERY label
    

let furthestNorthIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_vertices a WHERE y = (SELECT MAX(b.y) FROM spt_tsp_vertices b);"

let furthestSouthIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_vertices a WHERE y = (SELECT MIN(b.y) FROM spt_tsp_vertices b);"

let furthestEastIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_vertices a WHERE x = (SELECT MIN(b.x) FROM spt_tsp_vertices b);"

let furthestWestIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_vertices a WHERE x = (SELECT MAX(b.x) FROM spt_tsp_vertices b);"


let furthestNorthId : Script<int> = findIdQuery furthestNorthIdQUERY
    
let furthestSouthId : Script<int> = findIdQuery furthestSouthIdQUERY

let furthestEastId : Script<int> = findIdQuery furthestEastIdQUERY

let furthestWestId : Script<int> = findIdQuery furthestWestIdQUERY

type TspPrintRouteStepDict = 
    { CsvHeaders: string list
      MakeCsvRow: RouteNode -> RowWriter
    }

let generateTspRouteCsv (dict:TspPrintRouteStepDict) (startId:int) (endId:int) (outputFile:string) : Script<unit> =
    scriptMonad { 
        let! steps = eucledianTSP startId endId
        let rows = List.map  dict.MakeCsvRow steps
        let csvProc:CsvOutput<unit> = writeRowsWithHeaders dict.CsvHeaders rows
        do! liftAction <| outputToNew {Separator=","} csvProc outputFile
        }


let private getPoints (nodes:RouteNode list) : WGS84Point list = 
    List.map (fun x -> x.GridRef) nodes

let generateTspRouteWKT (startId:int) (endId:int) : Script<string> =
    scriptMonad { 
        let! steps = eucledianTSP startId endId
        let (points:WGS84Point list) = getPoints steps
        return (showWktLineString << WktLineString <| wgs84WktCoordList points)
        }