module Scripts.PathFinder

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open Npgsql

open SL.AnswerMonad
open SL.SqlUtils
open SL.PGSQLConn
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.GraphvizOutput
open SL.ScriptMonad


open Scripts.PostGIS


// ***** Set up the database
type EdgeDbRecord =
    { Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }


type EdgeInsertDict<'inputrow> = 
    { tryMakeEdgeDbRecord : 'inputrow -> EdgeDbRecord option }


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_pathfind"


/// We have a prepared statement to do this in a nice way, but calling it generates 
/// a error that I don't know how to fix.
let private makeEdgeDbInsert (edge1:EdgeDbRecord) : string = 
    let makePointLit (pt:WGS84Point) : string = 
        sprintf "ST_GeogFromText('SRID=4326;%s')" (showWktPoint <| wgs84WktPoint pt)
    // Note the id column is PG's SERIAL type so it is inserted automatically

    let makeDistanceLit (p1:WGS84Point) (p2:WGS84Point) : string = 
        sprintf "ST_Distance(%s,%s)"
                (makePointLit p1) 
                (makePointLit p2)
    sqlINSERT "spt_pathfind" 
            <|  [ stringValue       "basetype"          edge1.Basetype
                ; stringValue       "function_node"     edge1.FunctionNode
                ; literalValue      "start_point"       <| makePointLit edge1.StartPoint
                ; literalValue      "end_point"         <| makePointLit edge1.EndPoint
                ; literalValue      "distance_meters"   <| makeDistanceLit edge1.StartPoint edge1.EndPoint
                ]

let insertEdges (dict:EdgeInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    let proc1 (row:'inputrow) : PGSQLConn<int> = 
        match dict.tryMakeEdgeDbRecord row with
        | Some edge -> execNonQuery <|makeEdgeDbInsert edge
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

type PathTree<'edge> = 
    | PathTree of 'edge * PathTree<'edge> list
    

type Edge =
    { UID: int
      BaseType: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point
      DirectDistance: float<meter> }

//type GraphvizEdge = 
//    { StartId: string
//      EndId: string
//      LineStyle:string
//      EdgeId: string }

      
type Route<'edge> = Route of 'edge list

type Node<'gvId> = 
    { Name: string
      Location: WGS84Point
      NodeType: string
      StcId: string
      GvNodeId: 'gvId }



let makeFindEdgesQUERY (startPt:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            id, basetype, function_node, ST_AsText(end_point), distance_meters
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
        ; BaseType      = reader.GetString(1)
        ; FunctionNode  = reader.GetString(2)
        ; StartPoint    = startPt
        ; EndPoint      = wgs84End
        ; DirectDistance = 1.0<meter> * (float <| reader.GetDouble(4)) }
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
let allRoutes (allPaths:PathTree<'edge>) : Route<'edge> list = 
    let rec build (soFarRev:'a list) (currentTree:PathTree<'a>) : ('a list) list = 
        match currentTree with
        | PathTree(label,[]) -> [label :: soFarRev]
        | PathTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (Route << List.rev) <| build [] allPaths



let getSimpleRoutesFrom (startPt:WGS84Point) : Script<Route<Edge> list> = 
    fmapM (List.collect allRoutes) <| buildForest startPt

/// EdgeCache is (from,to) names
type EdgeCache = (string * string) list

let genDotEdges1 (route1: Route<string>) (edgeCache:EdgeCache) : GraphvizOutput<EdgeCache> = 
    let rec work cache x xs = 
        match xs with 
        | y :: ys -> 
            let current = (x,y)
            if List.exists (fun t -> t=current) cache then 
                work cache y ys
            else
                graphvizOutput { 
                    do! edge x y []
                    let! cache1 = work (current::cache) y ys 
                    return cache1
                }
        | [] -> graphvizOutput.Return cache
    match route1 with 
    | Route(x::xs) -> work edgeCache x xs 
    | Route _ -> graphvizOutput.Return edgeCache

let genDotEdges (routes: Route<string> list) : GraphvizOutput<unit> = 
    let rec work cache xs = 
        match xs with
        | [] -> graphvizOutput.Return ()
        | x :: xs -> 
            graphvizOutput { 
                let! cache1 = genDotEdges1 x cache
                do! work cache1 xs 
            }
    work [] routes

let generateDot (routes: Route<string> list) : string = 
    let procM : GraphvizOutput<unit> = 
        digraph "plan" 
            <| graphvizOutput { 
                    do! genDotEdges routes
                    return ()
                    }
    execGraphvizOutput procM
    
            
