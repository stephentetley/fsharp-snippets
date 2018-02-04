module Scripts.PathFinder

open Npgsql

open SL.AnswerMonad
open SL.SqlUtils
open SL.PGSQLConn
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.ScriptMonad

open Scripts.PostGIS


// ***** Set up the database
type EdgeInsert =
    { Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }


type EdgeInsertDict<'inputrow> = 
    { tryMakeEdgeInsert : 'inputrow -> EdgeInsert option }


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_pathfind"



let private makeEdgeInsert (edge1:EdgeInsert) : string = 
    let makePointLit (pt:WGS84Point) : string = 
        sprintf "ST_GeogFromText('SRID=4326;%s')" (showWktPoint <| wgs84WktPoint pt)
    // Note the id column is PG's SERIAL type so it is inserted automatically
    sqlINSERT "spt_pathfind" 
            <|  [ stringValue       "basetype"          edge1.Basetype
                ; stringValue       "function_node"     edge1.FunctionNode
                ; literalValue      "start_point"       <| makePointLit edge1.StartPoint
                ; literalValue      "end_point"         <| makePointLit edge1.EndPoint
                ]

let insertEdges (dict:EdgeInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    let proc1 (row:'inputrow) : PGSQLConn<int> = 
        match dict.tryMakeEdgeInsert row with
        | Some edge -> execNonQuery <| makeEdgeInsert edge
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 outfalls

let SetupEdgesDB (dict:EdgeInsertDict<'inputrow>) (edges:seq<'inputrow>) : Script<int> = 
    scriptMonad { 
        let! _ = deleteAllData () |> logScript (sprintf "%i rows deleted")
        let! count = insertEdges dict edges |> logScript (sprintf "%i rows inserted") 
        return count
     }


// ***** Path finding

type PathTree<'a> = 
    | PathTree of 'a * PathTree<'a> list
    


type Route<'a> = Route of 'a list


type Edge =
    { UID: int
      Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }

// SELECT id, basetype, function_node, ST_AsText(end_point) 
// FROM spt_pathfind 
// WHERE start_point = ST_GeomFromText('POINT(-2.16438 54.40591 )', 4326);
let makeFindEdgesQUERY (startPt:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            id, basetype, function_node, ST_AsText(end_point)
        FROM 
            spt_pathfind
        WHERE 
            start_point = ST_GeomFromText('{0}', 4326);
        """, showWktPoint <| wgs84WktPoint startPt)




let findEdges (startPt:WGS84Point) : Script<Edge list> = 
    let query = makeFindEdgesQUERY startPt
    let procM (reader:NpgsqlDataReader) : Edge = 
        let wgs84End = 
            match Option.bind wktPointToWGS84 <| tryReadWktPoint (reader.GetString(3)) with
            | Some pt -> pt
            | None -> failwith "findEdges - point not readable"
        { UID           = int <| reader.GetInt32(0)
        ; Basetype      = reader.GetString(1)
        ; FunctionNode  = reader.GetString(2)
        ; StartPoint    = startPt
        ; EndPoint      = wgs84End }
    liftWithConnParams << runPGSQLConn <| execReaderList query procM  

let notVisited (visited:Edge list) (e1:Edge) = 
    not <| List.exists (fun (e:Edge) -> e.UID = e1.UID) visited

/// A start-point may have many outward paths, hence we build a list of trees.
/// Note - if we study the data we should be able to prune the searches 
/// by looking at Function_link and only following paths that start with 
/// a particular link type.
let buildForest (startPt:WGS84Point) : Script<PathTree<Edge> list> = 
    let rec recBuild (pt:WGS84Point) (visited:Edge list) : Script<PathTree<Edge> list> = 
        scriptMonad { 
            // TO CHECK - Are we sure we are handling cyclic paths "wisely"?
            let! (esNew:Edge list) = fmapM (List.filter (notVisited visited)) <| findEdges pt
            let! branches = 
                forM esNew <| 
                    fun (e1:Edge) -> 
                        scriptMonad { 
                            let! kids = recBuild e1.EndPoint (e1::visited)
                            return (PathTree(e1,kids))
                        }
            return branches
        }
    recBuild startPt []      

    
// A Path tree cannot have cycles (they have been identified beforehand)...
let allRoutes (allPaths:PathTree<'a>) : Route<'a> list = 
    let rec build (soFarRev:'a list) (currentTree:PathTree<'a>) : ('a list) list = 
        match currentTree with
        | PathTree(label,[]) -> [label :: soFarRev]
        | PathTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (Route << List.rev) <| build [] allPaths

// Note to self - be careful using <| in computation expressions

let getRoutesFrom (startPt:WGS84Point) : Script<Route<Edge> list> = 
    fmapM (List.collect allRoutes) <| buildForest startPt




