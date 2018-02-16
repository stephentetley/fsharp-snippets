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




let private deleteAllTSPDbRows () : Script<int> = 
    liftWithConnParams 
        << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_tsp_nodes"


/// wgs84:Longitude is x, wgs84:Latitude is y
/// We have a label in the vertex table so we can interpret the data after the route 
/// is generated.
let private makeVertexINSERT (vertex:WGS84Point) (label:string) : string = 
    sqlINSERT "spt_tsp_nodes" 
            <|  [ floatValue    "x"         <| float vertex.Longitude
                ; floatValue    "y"         <| float vertex.Latitude
                ; stringValue   "label"     label
                ]


/// An asset has a label, it should have a location (but it might not).
type TspNodeInsertDict<'asset> = 
    { TryMakeNodeLocation : 'asset -> WGS84Point option
      MakeNodeLabel: 'asset -> string }


/// Design "problem" 
/// This procedure associates a row with an Id but the Id is (probably) unknown
/// to the user when it is returned from pgr_tsp, hence I think we need label 
/// and a join.
let insertVertices (dict:TspNodeInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    let proc1 (point:WGS84Point, label:string) : PGSQLConn<int> = 
        execNonQuery <| makeVertexINSERT point label
    
    let good1 (row:'row) : (WGS84Point * string) option = 
         Option.map (fun pt -> (pt, dict.MakeNodeLabel row)) 
            <| dict.TryMakeNodeLocation row

    let goodData = Seq.choose id <| Seq.map good1 vertices 

    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 goodData

let setupTspNodeDB (dict:TspNodeInsertDict<'row>) (vertices:seq<'row>) : Script<int> = 
    scriptMonad { 
        let! _      = deleteAllTSPDbRows ()         |> logScript (sprintf "%i rows deleted")
        let! count  = insertVertices dict vertices  |> logScript (sprintf "%i rows inserted") 
        return count
     }


let makeEucledianTSPQUERY (startId:int) (endId:int) : string = 
    System.String.Format("""
        SELECT r.seq, r.node, v.label, v.x, v.y, r.cost, r.agg_cost
        FROM 
            pgr_eucledianTSP( 'SELECT id, x, y FROM spt_tsp_nodes', 
                    start_id := {0},
                    end_id := {1}
                   ) AS r
        LEFT JOIN spt_tsp_nodes AS v ON r.node = v.id;
        """, startId, endId)

/// NodeLabel must be a short string so we can store it in the DB.
type TspRouteNode = 
    { SeqNumber: int
      NodeLabel: string
      GridRef: WGS84Point
      Cost: float
      AggCost: float }

type TspRoute = TspRouteNode list

let private dropLast (source:'a list) : 'a list = 
    let rec work ac xs = 
        match xs with
        | [] -> List.rev ac
        | [x] -> List.rev ac
        | x :: xs -> work (x::ac) xs
    work [] source

/// NOte - need at least two elements in the database.
let eucledianTSP (startId:int) (endId:int) : Script<TspRoute> = 
    let query = makeEucledianTSPQUERY startId endId
    let procM (reader:NpgsqlDataReader) : TspRouteNode = 
        let gridRef = 
            let lon = float <| reader.GetDouble(3)
            let lat = float <| reader.GetDouble(4)
            { Longitude = 1.0<degree> * lon; Latitude = 1.0<degree> * lat}
        { SeqNumber     = reader.GetInt32(0)
        ; NodeLabel     = reader.GetString(2)
        ; GridRef       = gridRef
        ; Cost          = float <| reader.GetDouble(5)
        ; AggCost       = float <| reader.GetDouble(6) } 
    fmapM dropLast << liftWithConnParams << runPGSQLConn <| execReaderList query procM  


let makeFindIdByLabelQUERY (label:string) : string = 
    System.String.Format("""
        SELECT id FROM spt_tsp_nodes WHERE label='{0}';
        """, label)

let private findIdQuery (query:string) : Script<int> = 
    let procM (reader:NpgsqlDataReader) : int = reader.GetInt32(0)
    liftWithConnParams << runPGSQLConn <| execReaderFirst query procM  


let findIdByLabel (label:string) : Script<int> = 
    findIdQuery <| makeFindIdByLabelQUERY label
    

let furthestNorthIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_nodes a WHERE y = (SELECT MAX(b.y) FROM spt_tsp_nodes b);"

let furthestSouthIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_nodes a WHERE y = (SELECT MIN(b.y) FROM spt_tsp_nodes b);"

let furthestEastIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_nodes a WHERE x = (SELECT MIN(b.x) FROM spt_tsp_nodes b);"

let furthestWestIdQUERY : string  = 
    "SELECT a.id FROM spt_tsp_nodes a WHERE x = (SELECT MAX(b.x) FROM spt_tsp_nodes b);"


let furthestNorthId () : Script<int> = findIdQuery furthestNorthIdQUERY
    
let furthestSouthId () : Script<int> = findIdQuery furthestSouthIdQUERY

let furthestEastId () : Script<int> = findIdQuery furthestEastIdQUERY

let furthestWestId () : Script<int> = findIdQuery furthestWestIdQUERY

type TspPrintRouteStepDict = 
    { CsvHeaders: string list
      MakeCsvRow: TspRouteNode -> RowWriter
    }

let generateTspRouteCsv (dict:TspPrintRouteStepDict) (startId:int) (endId:int) (outputFile:string) : Script<unit> =
    scriptMonad { 
        let! steps = eucledianTSP startId endId
        let rows = List.map dict.MakeCsvRow steps
        let csvProc:CsvOutput<unit> = writeRowsWithHeaders dict.CsvHeaders rows
        do! liftAction <| outputToNew {Separator=","} csvProc outputFile
        }


let private getPoints (nodes:TspRoute) : WGS84Point list = 
    List.map (fun x -> x.GridRef) nodes

let generateTspRouteWKT (startId:int) (endId:int) : Script<string> =
    scriptMonad { 
        let! steps = eucledianTSP startId endId
        let (points:WGS84Point list) = getPoints steps
        return (showWktLineString << WktLineString <| wgs84WktCoordList points)
        }

// New code...
// API of this module to think about at some point.

let tspRoute (dict:TspNodeInsertDict<'asset>) (assets:'asset list) : Script<(int * string) list> = 
    match assets with
    | [] -> scriptMonad.Return []
    | [one] -> scriptMonad.Return [(1, dict.MakeNodeLabel one)]
    | xs -> 
        scriptMonad { 
            let! _          = setupTspNodeDB dict assets
            let! startId    = furthestNorthId ()
            let! endId      = furthestSouthId ()
            let! steps      = eucledianTSP startId endId
            let  path       = List.map (fun (x:TspRouteNode) -> (x.SeqNumber, x.NodeLabel)) steps
            return path
            }

    