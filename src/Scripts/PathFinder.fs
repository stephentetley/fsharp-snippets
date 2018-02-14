module Scripts.PathFinder

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open System.Collections.Generic

open FSharpx.Collections

open Npgsql

open SL.AnswerMonad
open SL.NameGen
open SL.SqlUtils
open SL.PGSQLConn
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.GraphvizOutput
open SL.ScriptMonad


open Scripts.PostGIS

/// This is the representation of an edge that is stored in the DB.
type EdgeRecord =
    { UID: int
      TypeTag: string
      EdgeLabel: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point
      DirectDistance: float<meter> }

/// This is the representation of an node that is stored in the DB.
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
            | None -> failwith "findNode - point not readable"
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


/// A LinkTree is a branching route from a single source 
/// It actually stores edges (with start and end labels) rather
/// than nodes (with some unique id). 
/// Hence we also need a LinkForest to model multiple paths from 
/// the start node.
type LinkTree = 
    | LinkTree of EdgeRecord * LinkTree list

/// A source node may have more than one outgoing routes.
type LinkForest = LinkTree list


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
let buildLinkForest (startPt:WGS84Point) : Script<LinkForest> = 
    let rec recBuild (pt:WGS84Point) (visited:EdgeRecord list) : Script<LinkForest> = 
        scriptMonad { 
            // TO CHECK - Are we sure we are handling cyclic paths "wisely"?
            let! (esNew:EdgeRecord list) = 
                fmapM (List.filter (notVisited visited)) <| findOutwardEdges pt
            let! branches = 
                forM esNew <| 
                    fun (e1:EdgeRecord) -> 
                        scriptMonad { 
                            let! kids = recBuild e1.EndPoint (e1::visited)
                            return (LinkTree(e1,kids))
                        }
            return branches
        }
    recBuild startPt []      


// ***** User land representations of paths


// An internal represention of Node lookup from the database.
type DbNode = 
    | AnonNode of WGS84Point
    | ExtantNode of NodeRecord


// A LinkTree (LinkForest) is not user-friendly, nodes are fixed 
// to the DB representation.
//
// Two friendlier representations are a PathTree which stores 
// genuine nodes on its brances and a Route list that stores 
// linear routes.


/// A PathTree is a branching route from a single source.
/// Note this is a very "node-centric" view. We have no idea of 
/// edges so we cannot represent distances for example.
type PathTree<'node> = 
    | PathTree of 'node * PathTree<'node> list


let makePathTree (linkTrees:LinkForest) : Script<PathTree<DbNode> option> =
    let assertStart (trees:LinkTree list) : EdgeRecord option = 
        match trees with
        | [] -> None
        | LinkTree(start,kids)  :: xs -> 
            if List.forall (fun (LinkTree(s1,_)) -> start.UID = s1.UID ) xs then
                Some start
            else None

    let findDbNode (pt:WGS84Point) : Script<DbNode> = 
        scriptMonad { 
            let! opt = findNearestNode pt 1.0<meter>
            match opt with
            | None -> return (AnonNode pt)
            | Some node -> return (ExtantNode node) 
            }

    let rec buildTree (pt:WGS84Point) (kids:LinkTree list) : Script<PathTree<DbNode>> = 
        scriptMonad {
            let! node1 = findDbNode pt
            let! kids1 = 
                forM kids 
                    (fun (tree:LinkTree) ->
                        match tree with
                        | LinkTree(edge,children) -> buildTree edge.EndPoint children)                      
            return PathTree(node1,kids1)
            }
    scriptMonad { 
        let start = assertStart linkTrees
        match start with
        | None -> return None
        | Some node1 -> 
            let! tree = buildTree node1.StartPoint linkTrees
            return (Some tree)
        }

/// Not tail-recursive...
let mapPathTree (fn:'a -> 'b) (pathTree:PathTree<'a>) : PathTree<'b> =
    let rec work tree = 
        match tree with
        | PathTree(node,[]) -> PathTree (fn node, [])
        | PathTree(node,kids) -> PathTree(fn node, List.map work kids)
    work pathTree
    

/// A Route is a so-called "Fence post list":
/// node
/// node-edge-node
/// node-edge-node-edge-node
/// etc.
type Route<'node,'edge> = 
    { StartPoint: 'node
      Steps: ('edge * 'node) list }

type InternalRoute = Route<DbNode,EdgeRecord>


// Below is old code...

// Note - we could recover a PathTree<'realNode> from DbPathForest 
// because DbPathForest stores edges in the node position. 
// Each initial edge should have the same start point

//let private anonTag : string = "###ANON"

//let private isAnonNode (node:NodeRecord) : bool =
//    node.UID = -1 && node.TypeTag = anonTag

//let private anonNode (gridRef:WGS84Point) : NodeRecord =
//    { UID       = -1
//      NodeLabel = ""
//      TypeTag   = anonTag
//      GridRef   = gridRef }








type EdgeRecordPath = EdgeRecord list

//// A Path tree cannot have cycles (they have been identified beforehand)...
let private getEdgeRecordPaths (linkTree:LinkTree) : EdgeRecordPath list = 
    let rec build (soFarRev:EdgeRecord list) (currentTree:LinkTree) : (EdgeRecord list) list = 
        match currentTree with
        | LinkTree(label,[]) -> [label :: soFarRev]
        | LinkTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (List.rev) <| build [] linkTree

type private DbTail = (EdgeRecord * DbNode) list

let private edgeRecordPathToDbRoute (edgePath:EdgeRecordPath) : Script<InternalRoute option> = 
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
                                    | Some pt-> ExtantNode pt 
                                    | None -> AnonNode x.EndPoint
                                buildTail ((x,endPt)::ac) xs)
    scriptMonad { 
        let! a = startNode edgePath
        match a with
        | None -> return None
        | Some n1 -> 
            let! rest = buildTail [] edgePath
            return (Some { StartPoint = ExtantNode n1; Steps = rest})
        }

let private allRoutesTree (pathTree:LinkTree) : Script<InternalRoute list> = 
    fmapM (List.choose id) << mapM edgeRecordPathToDbRoute <| getEdgeRecordPaths pathTree

let allRoutes (forest:LinkForest) : Script<InternalRoute list> = 
    fmapM List.concat <| mapM allRoutesTree forest

// NOTE 
// We can't translate the PathTree (without querying the 
// database), as it does not carry enough information.
// It only has a notion of node, although to build the tree from 
// the database we actually store edges.

/// EdgeRecord does not store much information, so to make an 
/// edge we need to look at its start and end nodes.
/// Also grid_ref is not enough information to make an anon node
/// e.g. for graphviz we need something that generates a good id 
/// so we use NameGen.
type DeriveRouteDict<'node,'edge> = 
    { MakeRouteNode: NodeRecord -> NameGen<'node>
      MakeAnonNode: WGS84Point -> NameGen<'node>
      MakeRouteEdge: 'node -> 'node -> EdgeRecord -> NameGen<'edge> }



type private NodeCache<'node> = Dictionary<int,'node>

let private cacheLookup(cache:NodeCache<'node>) (ix:int) : 'node option = 
    Dictionary.tryFind ix cache

/// Mutable!
let private cacheAdd(cache:NodeCache<'node>) (ix:int) (node:'node) : unit =    
    cache.Add(ix,node) 


let private translateRoute (dict:DeriveRouteDict<'node,'edge>) (cache:NodeCache<'node>) (dbRoute:InternalRoute) : NameGen<Route<'node,'edge>> = 
    let makeNode (dbNode:DbNode) : NameGen<'node> = 
        match None with    // match cacheLookup cache dbNode.UID with
        | Some node -> nameGen.Return node
        | None ->
            nameGen 
                { let! node =   
                    match dbNode with
                    | AnonNode pt -> dict.MakeAnonNode pt
                    | ExtantNode node -> dict.MakeRouteNode node
                  // let _ = cacheAdd cache dbNode.UID node
                  return node }
    
    let rec makeSteps prev ac steps =
        match steps with
        | [] -> nameGen.Return (List.rev ac)
        | (e,n) :: zs -> 
            nameGen.Bind ( makeNode n
                         , fun next -> 
                            nameGen.Bind ( dict.MakeRouteEdge prev next e,
                                           fun edge -> makeSteps next ((edge,next)::ac) zs))
    nameGen {
        let! start = makeNode dbRoute.StartPoint
        let! steps = makeSteps start [] dbRoute.Steps
        return { StartPoint = start; Steps = steps } 
        }
    


let extractAllRoutes (dict:DeriveRouteDict<'node,'edge>) (forest:LinkForest) : Script<Route<'node,'edge> list> = 
    let cache = new Dictionary<int,'node> ()
    scriptMonad { 
        let! dbRoutes   = fmapM List.concat <| mapM allRoutesTree forest
        let userRoutes  = 
            runNameGenOne (sprintf "node%i") <| SL.NameGen.mapM (translateRoute dict cache) dbRoutes
        return userRoutes 
        }


let edgeListFromRoute(route:Route<'node,'edge>) : 'edge list = 
    let rec work ac steps = 
        match steps with 
        | [] -> List.rev ac
        | (edge,node) :: zs -> work (edge::ac) zs
    work [] route.Steps

let nodeListFromRoute(route:Route<'node,'edge>) : 'node list = 
    let rec work ac steps = 
        match steps with 
        | [] -> List.rev ac
        | (edge,node) :: zs -> work (node::ac) zs
    work [route.StartPoint] route.Steps



// ***** Graphviz


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

let makeEdgeLabel (dist:float<meter>) : string = sprintf "%.2fkm" (0.001*float dist)

let graphvizDict : DeriveRouteDict<GraphvizNode, GraphvizEdge> = 
    let genNode (node:NodeRecord) = 
        nameGen { 
            let! nodeId = newName ()
            return { NodeId = nodeId;
                     NodeLabel = node.NodeLabel;
                      Shape = "box"; FillColor = None }
            }
    let genAnonNode (gridRef:WGS84Point) = 
        nameGen { 
            let! nodeId = newName ()
            return { NodeId = nodeId; 
                     NodeLabel = gridRef.ToString(); 
                     Shape = "box"; FillColor = None }
            }
    let genEdge (prev:GraphvizNode) (next:GraphvizNode) (edge:EdgeRecord) = 
        nameGen.Return { StartId = prev.NodeId; EndId = next.NodeId;
                         LineStyle = None; LineColour = None; 
                         EdgeLabel = Some <| makeEdgeLabel edge.DirectDistance }
    { MakeRouteNode = genNode
    ; MakeAnonNode  = genAnonNode
    ; MakeRouteEdge = genEdge }


let genDotNode (node1:GraphvizNode) : GraphvizOutput<unit> = 
    SL.GraphvizOutput.node node1.NodeId [label node1.NodeLabel]

/// EdgeCache is (from,to) names
type EdgeCache = (string * string) list


let genDotEdges1 (edgeCache:EdgeCache) (edges: GraphvizEdge list)  : GraphvizOutput<EdgeCache> = 
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
    work edgeCache edges 


let genDotEdges (allEdgeLists: (GraphvizEdge list) list) : GraphvizOutput<unit> = 
    let rec work cache xs =
        match xs with
        | [] -> graphvizOutput.Return ()
        | x :: xs -> 
            graphvizOutput { 
                let! cache1 = genDotEdges1 cache x
                do! work cache1 xs 
            }
    work [] allEdgeLists

type Rank = GraphvizNode list

/// Note transpose supplied by FSarpx.Collections...
let transpose (source: ('a list) list) : ('a list) list = 
    let heads = 
        List.choose (fun xs -> match xs with | [] -> None | (x::_) -> Some x)
    let tails = 
        List.choose (fun xs -> match xs with | [] -> None | (_::xs) -> Some xs)
    let rec work zss = 
        match zss with
        | [] -> []
        | [] :: xss -> work xss
        | (x::xs) :: xss -> 
            let front = x :: heads xss
            let rest  = work <| xs :: tails xss
            front::rest
    work source

let compactRank (rank:Rank) : Rank = 
    let rec work (ac:GraphvizNode list) (input:GraphvizNode list) =
        match input with
        | [] -> List.rev ac
        | x :: xs -> 
            if List.exists (fun (a:GraphvizNode) -> x.NodeId = a.NodeId) ac then
                work ac xs
            else work (x::ac) xs
    work [] rank



let private rankRoutes (routes: Route<GraphvizNode, GraphvizEdge> list) : Rank list = 
    List.map compactRank << transpose <| List.map nodeListFromRoute routes


let genDotRanks (ranks:Rank list) : GraphvizOutput<unit> = 
    let rankProc (rank1:Rank) : GraphvizOutput<unit> = 
        anonSubgraph 
            <| graphvizOutput { 
                do! attrib <| rank "same"
                do! SL.GraphvizOutput.mapMz genDotNode rank1
                }
    SL.GraphvizOutput.mapMz rankProc ranks
    

let generateDot (graphName:string) (routes: Route<GraphvizNode, GraphvizEdge> list) : GraphvizOutput<unit> = 
    let paths = List.map edgeListFromRoute routes
    let ranks = rankRoutes routes
    digraph graphName
            <| graphvizOutput { 
                    do! attrib <| rankdir LR
                    do! genDotRanks ranks
                    do! genDotEdges paths
                    return ()
                    }

    
            
