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



type EdgeRecord =
    { UID: int
      TypeTag: string
      EdgeLabel: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point
      DirectDistance: float<meter> }

type NodeRecord =
    { UID: int
      NodeLabel: string
      TypeTag: string 
      GridRef: WGS84Point }


// ***** Set up the database

/// Distance and UID are opaque to the user, so the record users have to 
/// create from input omits them.
type UserLandEdge =
    { TypeTag: string
      EdgeLabel: string
      EdgeStart: WGS84Point
      EdgeEnd: WGS84Point }

/// UID is opaque to the user, so the record users have to create from input 
/// data omits it.
type UserLandNode =
    { TypeTag: string 
      NodeLabel: string
      NodeLocation: WGS84Point }


type PathFindInsertDict<'node,'edge> = 
    { tryMakeUserLandNode: 'node -> UserLandNode option 
      tryMakeUserLandEdge: 'edge -> UserLandEdge option }


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| 
        pgsqlConn { 
            let! i = deleteAllRowsRestartIdentity "spt_pathfind_edges"
            let! j = deleteAllRowsRestartIdentity "spt_pathfind_nodes"
            return (i+j)
            }

let private makePointLit (pt:WGS84Point) : string = 
    sprintf "ST_GeogFromText('SRID=4326;%s')" (showWktPoint <| wgs84WktPoint pt)

let private makeNodeInsertStmt (node1:UserLandNode) : string = 
    sqlINSERT "spt_pathfind_nodes" 
        <|  [ stringValue       "type_tag"          node1.TypeTag
            ; stringValue       "node_label"        node1.NodeLabel
            ; literalValue      "grid_ref"          <| makePointLit node1.NodeLocation
            ]


/// We have a prepared statement to do this in a nice way, but calling it generates 
/// a error that I don't know how to fix.
let private makeEdgeInsertStmt (edge1:UserLandEdge) : string = 
    // Note the id column is PG's SERIAL type so it is inserted automatically
    let makeDistanceProc (p1:WGS84Point) (p2:WGS84Point) : string = 
        sprintf "ST_Distance(%s,%s)"
                (makePointLit p1) 
                (makePointLit p2)

    sqlINSERT "spt_pathfind_edges" 
        <|  [ stringValue       "type_tag"          edge1.TypeTag
            ; stringValue       "edge_label"        edge1.EdgeLabel
            ; literalValue      "start_point"       <| makePointLit edge1.EdgeStart
            ; literalValue      "end_point"         <| makePointLit edge1.EdgeEnd
            ; literalValue      "distance_meters"   <| makeDistanceProc edge1.EdgeStart edge1.EdgeEnd
            ]


let insertNodes (dict:PathFindInsertDict<'noderow,'edgerow>) (source:seq<'noderow>) : Script<int> = 
    let proc1 (row:'noderow) : PGSQLConn<int> = 
        match dict.tryMakeUserLandNode row with
        | Some node -> execNonQuery <| makeNodeInsertStmt node
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 source

let insertEdges (dict:PathFindInsertDict<'noderow,'edgerow>) (source:seq<'edgerow>) : Script<int> = 
    let proc1 (row:'edgerow) : PGSQLConn<int> = 
        match dict.tryMakeUserLandEdge row with
        | Some edge -> execNonQuery <| makeEdgeInsertStmt edge
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 source



let SetupPathsDB (dict:PathFindInsertDict<'noderow,'edgerow>) (nodes:seq<'noderow>) (edges:seq<'edgerow>) : Script<int> = 
    scriptMonad { 
        let! _ = deleteAllData ()           |> logScript (sprintf "%i rows deleted")
        let! c1 = insertNodes dict nodes    |> logScript (sprintf "%i node rows inserted")
        let! c2 = insertEdges dict edges    |> logScript (sprintf "%i edge rows inserted") 
        return (c1 + c2)
     }

// ***** Node finding 
// e.g for start node

let private findNodeQUERY (typeTag:string) (nodeLabel:string) : string = 
    System.String.Format("""
        SELECT 
            id, type_tag, node_label, ST_AsText(grid_ref) as wkt
        FROM 
            spt_pathfind_nodes
        WHERE
            type_tag = '{0}' AND node_label='{1}';
        """, typeTag, nodeLabel)

let findNode (typeTag:string) (nodeLabel:string) : Script<NodeRecord> = 
    let query = findNodeQUERY typeTag nodeLabel
    let procM (reader:NpgsqlDataReader) : NodeRecord = 
        let gridRef = 
            match Option.bind wktPointToWGS84 <| tryReadWktPoint (reader.GetString(3)) with
            | Some pt -> pt
            | None -> failwith "findEdges - point not readable"
        { UID           = int <| reader.GetInt32(0)
        ; TypeTag       = reader.GetString(1)
        ; NodeLabel     = reader.GetString(2)
        ; GridRef       = gridRef }
    liftWithConnParams << runPGSQLConn <| execReaderFirst query procM  

let private findNearestNodeQUERY (gridRef:WGS84Point) (proximity:float<meter>) : string = 
    System.String.Format("""
        SELECT 
            o.id, o.type_tag, o.node_label, 
            ST_AsText(o.grid_ref) as wkt, 
            ST_Distance(o.grid_ref, ST_Point({0}, {1})) as dist
        FROM
            spt_pathfind_nodes o
        WHERE 
            ST_Distance(o.grid_ref, ST_Point({0},{1})) < {2}
        ORDER BY
            o.grid_ref <-> ST_Point({0}, {1}) :: geography limit 1;
        """, gridRef.Longitude, gridRef.Latitude, float proximity )

let findNearestNode (origin:WGS84Point) (proximity:float<meter>) : Script<NodeRecord option> = 
    let query = findNearestNodeQUERY origin proximity
    let procM (reader:NpgsqlDataReader) : NodeRecord option = 
        let makeNode gridRef = 
            { UID           = int <| reader.GetInt32(0)
            ; TypeTag       = reader.GetString(1)
            ; NodeLabel     = reader.GetString(2)
            ; GridRef       = gridRef }
        Option.map makeNode 
            << Option.bind wktPointToWGS84
            <| tryReadWktPoint (reader.GetString(3))

    liftWithConnParams << runPGSQLConn <| execReaderFirst query procM  



// ***** Path finding


/// A PathTree is a branching route from a single source 
type PathTree<'node> = 
    | PathTree of 'node * PathTree<'node> list

/// A source node may have more than one outgoing routes.
type PathForest<'node> = PathTree<'node> list


type DbPathTree     = PathTree<EdgeRecord>
type DbPathForest   = PathForest<EdgeRecord>


let makeFindEdgesQUERY (startPt:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            id, type_tag, edge_label, ST_AsText(end_point), distance_meters
        FROM 
            spt_pathfind_edges
        WHERE 
            start_point = ST_GeomFromText('{0}', 4326);
        """, showWktPoint <| wgs84WktPoint startPt)




let findOutwardEdges (startPt:WGS84Point) : Script<EdgeRecord list> = 
    let query = makeFindEdgesQUERY startPt
    let procM (reader:NpgsqlDataReader) : EdgeRecord = 
        let wgs84End = 
            match Option.bind wktPointToWGS84 <| tryReadWktPoint (reader.GetString(3)) with
            | Some pt -> pt
            | None -> failwith "findEdges - point not readable"
        { UID           = int <| reader.GetInt32(0)
        ; TypeTag       = reader.GetString(1)
        ; EdgeLabel     = reader.GetString(2)
        ; StartPoint    = startPt
        ; EndPoint      = wgs84End
        ; DirectDistance = 1.0<meter> * (float <| reader.GetDouble(4)) }
    liftWithConnParams << runPGSQLConn <| execReaderList query procM  

let notVisited (visited:EdgeRecord list) (e1:EdgeRecord) = 
    not <| List.exists (fun (e:EdgeRecord) -> e.UID = e1.UID) visited



/// A start-point may have many outward paths, hence we build a list of trees.
/// Note - if we study the data we should be able to prune the searches 
/// by looking at Function_link and only following paths that start with 
/// a particular link type.
let buildForest (startPt:WGS84Point) : Script<DbPathForest> = 
    let rec recBuild (pt:WGS84Point) (visited:EdgeRecord list) : Script<DbPathForest> = 
        scriptMonad { 
            // TO CHECK - Are we sure we are handling cyclic paths "wisely"?
            let! (esNew:EdgeRecord list) = 
                fmapM (List.filter (notVisited visited)) <| findOutwardEdges pt
            let! branches = 
                forM esNew <| 
                    fun (e1:EdgeRecord) -> 
                        scriptMonad { 
                            let! kids = recBuild e1.EndPoint (e1::visited)
                            return (PathTree(e1,kids))
                        }
            return branches
        }
    recBuild startPt []      

let private anonTag : string = "###ANON"

let private isAnonNode (node:NodeRecord) : bool =
    node.UID = -1 && node.TypeTag = anonTag

let private anonNode (gridRef:WGS84Point) : NodeRecord =
    { UID       = -1
      NodeLabel = ""
      TypeTag   = anonTag
      GridRef   = gridRef }


type Route<'node,'edge> = 
    { StartPoint: 'node
      Steps: ('edge * 'node) list }

type DbRoute = Route<NodeRecord,EdgeRecord>

type EdgeRecordPath = EdgeRecord list

//// A Path tree cannot have cycles (they have been identified beforehand)...
let private getEdgeRecordPaths (pathTree:DbPathTree) : EdgeRecordPath list = 
    let rec build (soFarRev:'a list) (currentTree:PathTree<'a>) : ('a list) list = 
        match currentTree with
        | PathTree(label,[]) -> [label :: soFarRev]
        | PathTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (List.rev) <| build [] pathTree

type private DbTail = (EdgeRecord * NodeRecord) list

let private edgeRecordPathToDbRoute (edgePath:EdgeRecordPath) : Script<DbRoute option> = 
    let startNode (edges:EdgeRecordPath) : Script<NodeRecord option> = 
        match edges with
        | x :: _ -> findNearestNode x.StartPoint 1.0<meter>
        | _ -> scriptMonad.Return None
    
    let rec buildTail (ac:DbTail) (edges:EdgeRecordPath) : Script<DbTail> = 
        match edges with 
        | [] -> scriptMonad.Return (List.rev ac)
        | x :: xs -> 
            scriptMonad.Bind ( findNearestNode x.EndPoint 1.0<meter>
                             , fun opt -> 
                                let endPt = 
                                    match opt with 
                                    | Some pt-> pt 
                                    | None -> anonNode x.EndPoint
                                buildTail ((x,endPt)::ac) xs)
    scriptMonad { 
        let! a = startNode edgePath
        match a with
        | None -> return None
        | Some n1 -> 
            let! rest = buildTail [] edgePath
            return (Some { StartPoint = n1; Steps = rest})
        }

let allRoutesTree (pathTree:DbPathTree) : Script<DbRoute list> = 
    fmapM (List.choose id) << mapM edgeRecordPathToDbRoute <| getEdgeRecordPaths pathTree

let allRoutesForest (forest:DbPathForest) : Script<DbRoute list> = 
    fmapM List.concat <| mapM allRoutesTree forest


type DeriveRouteDict<'node,'edge> = 
    { makeRouteNode: NodeRecord -> 'node
      makeAnonNode: WGS84Point -> 'node
      makeRouteEdge: EdgeRecord -> 'edge }


let deriveRoute (dict:DeriveRouteDict<'node,'edge>) (dbRoute:DbRoute) : Route<'node,'edge> = 
    let makeNode (dbNode:NodeRecord) : 'node = 
        if isAnonNode dbNode then
            dict.makeAnonNode dbNode.GridRef
        else dict.makeRouteNode dbNode

    { StartPoint = makeNode dbRoute.StartPoint
    ; Steps = List.map (fun (e,n) -> (dict.makeRouteEdge e, makeNode n)) dbRoute.Steps } 



/// A route is really a node list, though we we can treat an edge as a node
/// if we principally consider its start point.
type RouteOld<'node> = RouteOld of 'node list


/// An edge list must provide access to start and end 
/// (e.g. as coordinates or ids).
type EdgeList<'edge> = EdgeList of 'edge list

    
// A Path tree cannot have cycles (they have been identified beforehand)...
let allRoutesTreeOld (pathTree:PathTree<'node>) : RouteOld<'node> list = 
    let rec build (soFarRev:'a list) (currentTree:PathTree<'a>) : ('a list) list = 
        match currentTree with
        | PathTree(label,[]) -> [label :: soFarRev]
        | PathTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (RouteOld << List.rev) <| build [] pathTree

let allRoutesForestOld (allTrees:PathForest<'edge>) : RouteOld<'edge> list = 
    List.collect allRoutesTreeOld allTrees

let getSimpleRoutesFrom (startPt:WGS84Point) : Script<RouteOld<EdgeRecord> list> = 
    fmapM allRoutesForestOld <| buildForest startPt




// ***** Graphviz


type MakeEdgeDict<'node,'edge> = 
    { MakeEdgeFromRouteNodes: 'node -> 'node -> 'edge }

let routeToEdgeList (dict:MakeEdgeDict<'node,'edge>) (route:RouteOld<'node>) : EdgeList<'edge> = 
    let rec work ac node1 nodes =
        match nodes with
        | [] -> List.rev ac     
        | [node2] -> 
            let edge1 = dict.MakeEdgeFromRouteNodes node1 node2
            List.rev (edge1::ac)
        | node2 ::ns -> 
            let edge1 = dict.MakeEdgeFromRouteNodes node1 node2
            work (edge1::ac) node2 ns
    match route with
    | RouteOld (x::xs) -> EdgeList <| work [] x xs
    | _ -> EdgeList []




type GraphvizEdge = 
    { StartId: string
      EndId: string
      LineStyle: string option
      LineColour:string option
      EdgeLabel: string option }


type GraphvizNode = 
    { NodeId: string
      NodeLabel: string
      Shape: string
      FillColor: string option }


// Note - we have to use the Postgres UID if we want to get a
// GraphvizEdge from an Edge without extra lookups...

let edgeToGraphvizEdgeDict:MakeEdgeDict<EdgeRecord,GraphvizEdge> = 
        let makeLabel (dist:float<meter>) : string = sprintf "%.2fkm" (0.001*float dist)
 
        { MakeEdgeFromRouteNodes = 
            fun n1 n2 -> { StartId = sprintf "node%i" n1.UID; 
                            EndId = sprintf "node%i" n2.UID; 
                            LineStyle = None;
                            LineColour= Some "red1"; 
                            EdgeLabel = Some <| makeLabel n1.DirectDistance }
        }




/// EdgeCache is (from,to) names
type EdgeCache = (string * string) list


let genDotEdges1 (path1: EdgeList<GraphvizEdge>) (edgeCache:EdgeCache) : GraphvizOutput<EdgeCache> = 
    let rec work (cache:EdgeCache) (edges:GraphvizEdge list) = 
        match edges with 
        | x :: xs -> 
            let current = (x.StartId, x.EndId)
            if List.exists (fun t -> t=current) cache then 
                work cache xs
            else
                graphvizOutput {
                    let attrs = 
                        List.choose id [ Option.map label x.EdgeLabel;
                                         Option.map (fun z -> style [z]) x.LineStyle;
                                         Option.map color x.LineColour ]
                    do! edge x.StartId x.EndId attrs // [label x.EdgeLabel; style [x.LineStyle]; color x.LineColour]
                    let! cache1 = work (current::cache) xs 
                    return cache1
                }
        | [] -> graphvizOutput.Return cache
    match path1 with 
    | EdgeList xs -> work edgeCache xs 


let genDotEdges (paths: EdgeList<GraphvizEdge> list) : GraphvizOutput<unit> = 
    let rec work cache xs = 
        match xs with
        | [] -> graphvizOutput.Return ()
        | x :: xs -> 
            graphvizOutput { 
                let! cache1 = genDotEdges1 x cache
                do! work cache1 xs 
            }
    work [] paths

let generateDot  (graphName:string) (paths: EdgeList<GraphvizEdge> list) : GraphvizOutput<unit> = 
    digraph graphName
            <| graphvizOutput { 
                    do! attrib <| rankdir LR
                    do! genDotEdges paths
                    return ()
                    }

    
            
