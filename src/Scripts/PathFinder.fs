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
open SL.PostGIS

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
    { TryMakeUserLandNode: 'node -> UserLandNode option 
      TryMakeUserLandEdge: 'edge -> UserLandEdge option }


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
        match dict.TryMakeUserLandNode row with
        | Some node -> execNonQuery <| makeNodeInsertStmt node
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 source

let insertEdges (dict:PathFindInsertDict<'noderow,'edgerow>) (source:seq<'edgerow>) : Script<int> = 
    let proc1 (row:'edgerow) : PGSQLConn<int> = 
        match dict.TryMakeUserLandEdge row with
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


// A LinkForest is not user-friendly, nodes are fixed to the DB 
// representation.
//
// Two friendlier representations are a PathTree which stores 
// genuine nodes on its brances and a RouteList that stores 
// linear routes.


// ****************************************************************************
// ***** PATH TREE

/// A PathTree is a branching route from a single source.
/// Note this is a very "node-centric" view. We have no idea of 
/// edges so we cannot represent edge labels, distances etc.
type PathTree<'node> = 
    | PathTree of 'node * PathTree<'node> list


/// User must supply a dictionary to translate individual nodes.
type ExtractPathTreeDict<'node> = 
    { MakePathTreeNode: NodeRecord -> NameGen<'node>
      MakePathTreeAnonNode: WGS84Point -> NameGen<'node> }


let private makePathTree (forest:LinkForest) : Script<PathTree<DbNode>> =
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
        let start = assertStart forest
        match start with
        | None -> return (failwith "makePathTree - cahnge this to throwError")
        | Some node1 -> 
            let! tree = buildTree node1.StartPoint forest
            return tree
        }

/// Not tail-recursive...
let private mapPathTreeM (fn:'a -> NameGen<'b>) (pathTree:PathTree<'a>) : NameGen<PathTree<'b>> =
    let rec work tree = 
        nameGen { 
            match tree with
            | PathTree(node,[]) -> 
                let! node1 = fn node
                return PathTree (node1, [])
            | PathTree(node,kids) -> 
                let! node1 = fn node
                let! kids1 = SL.NameGen.mapM work kids
                return PathTree(node1,kids1)
            }
    work pathTree

    
let extractPathTree (dict:ExtractPathTreeDict<'node>) (forest:LinkForest) : Script<PathTree<'node>> =
    let convertNode (dbNode:DbNode) : NameGen<'node> = 
        match dbNode with
        | AnonNode pt -> dict.MakePathTreeAnonNode pt 
        | ExtantNode node -> dict.MakePathTreeNode node
    scriptMonad {
        let! dbPathTree = makePathTree forest
        let pathTree    = runNameGenOne (sprintf "node%i") (mapPathTreeM convertNode dbPathTree)
        return pathTree
        }

// ***** ROUTE / ROUTE LIST

/// A Route is a so-called "Fence post list":
/// node
/// node-edge-node
/// node-edge-node-edge-node
/// etc.
type Route<'node,'edge> = 
    { StartPoint: 'node
      Steps: ('edge * 'node) list }





// NOTE 
// We can't directly translate the LinkForest (we have to query
// the database as we go).

// This is because the LinkForest does not carry enough 
// information. It only has a notion of node, although to build 
// the tree from the database we actually store edges (at the 
// node position).



/// We have to make the route from the LinkForest and the
/// database. Unfortunately we can't make it from a PathTree
/// because PathTree has lost some important link information.
/// This has can introduce consistency problems for operations
/// that use both representations (e.g. generating Graphviz)
/// where we have to be careful to use the same method to 
/// generate node names.

type InternalDbRoute = Route<DbNode,EdgeRecord>


/// ExtractRouteDict
/// EdgeRecord does not store much information, so to make an 
/// edge we need to look at its start and end nodes.
/// Also grid_ref is not enough information to make an anon node
/// e.g. for graphviz we need something that generates a good id 
/// so we use NameGen.

/// User must supply a dictionary to translate individual nodes.
type ExtractRouteDict<'node,'edge> = 
    { MakeRouteNode: NodeRecord -> NameGen<'node>
      MakeRouteAnonNode: WGS84Point -> NameGen<'node>
      MakeRouteEdge: 'node -> 'node -> EdgeRecord -> NameGen<'edge> }



type EdgeList = EdgeRecord list

//// A Link tree cannot have cycles (they have been identified beforehand)...
let private getEdgeLists (linkTree:LinkTree) : EdgeList list = 
    let rec build (soFarRev:EdgeRecord list) (currentTree:LinkTree) : (EdgeRecord list) list = 
        match currentTree with
        | LinkTree(label,[]) -> [label :: soFarRev]
        | LinkTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (List.rev) <| build [] linkTree


type private DbTail = (EdgeRecord * DbNode) list

let private edgeListToDbRoute (edgeList:EdgeList) : Script<InternalDbRoute option> = 
    let startNode (edges:EdgeList) : Script<NodeRecord option> = 
        match edges with
        | x :: _ -> findNearestNode x.StartPoint 1.0<meter>
        | _ -> scriptMonad.Return None
    
    let rec buildTail (ac:DbTail) (edges:EdgeList) : Script<DbTail> = 
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
        let! a = startNode edgeList
        match a with
        | None -> return None
        | Some n1 -> 
            let! rest = buildTail [] edgeList
            return (Some { StartPoint = ExtantNode n1; Steps = rest})
        }

let private linkTreeRoutes (linkTree:LinkTree) : Script<InternalDbRoute list> = 
    fmapM (List.choose id) << mapM edgeListToDbRoute <| getEdgeLists linkTree

let private linkForestRoutes (forest:LinkForest) : Script<InternalDbRoute list> = 
    fmapM List.concat <| mapM linkTreeRoutes forest


type private NodeCache<'node> = Dictionary<int,'node>

let private cacheLookup(cache:NodeCache<'node>) (ix:int) : 'node option = 
    Dictionary.tryFind ix cache

/// Mutable!
let private cacheAdd(cache:NodeCache<'node>) (ix:int) (node:'node) : unit =    
    cache.Add(ix,node) 


let private translateRoute (dict:ExtractRouteDict<'node,'edge>) (cache:NodeCache<'node>) (dbRoute:InternalDbRoute) : NameGen<Route<'node,'edge>> = 
    let makeNode (dbNode:DbNode) : NameGen<'node> = 
        match None with    // match cacheLookup cache dbNode.UID with
        | Some node -> nameGen.Return node
        | None ->
            nameGen 
                { let! node =   
                    match dbNode with
                    | AnonNode pt -> dict.MakeRouteAnonNode pt
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
    


let extractAllRoutes (dict:ExtractRouteDict<'node,'edge>) (forest:LinkForest) : Script<Route<'node,'edge> list> = 
    let cache = new Dictionary<int,'node> ()
    scriptMonad { 
        let! dbRoutes   = linkForestRoutes forest
        let userRoutes  = 
            runNameGenOne (sprintf "node%i") <| SL.NameGen.mapM (translateRoute dict cache) dbRoutes
        return userRoutes 
        }


// TODO - Maybe these should be in a user API but at the moment
// exposing them is confusing...

let private edgeListFromRoute(route:Route<'node,'edge>) : 'edge list = 
    let rec work ac steps = 
        match steps with 
        | [] -> List.rev ac
        | (edge,node) :: zs -> work (edge::ac) zs
    work [] route.Steps

let private nodeListFromRoute(route:Route<'node,'edge>) : 'node list = 
    let rec work ac steps = 
        match steps with 
        | [] -> List.rev ac
        | (edge,node) :: zs -> work (node::ac) zs
    work [route.StartPoint] route.Steps


// ****************************************************************************
// ***** Graphviz


type GraphvizEdge = 
    { StartId: string
      EndId: string
      LineStyle: string option
      LineColour:string option
      EdgeLabel: string option }


[<StructuredFormatDisplay("{NodeId} {NodeLabel}")>]
type GraphvizNode = 
    { NodeId: string
      NodeLabel: string
      Shape: string
      FillColor: string option }
    

let makeEdgeLabel (dist:float<meter>) : string = sprintf "%.2fkm" (0.001*float dist)

let graphvizDict : ExtractRouteDict<GraphvizNode, GraphvizEdge> = 
    let genNode (node:NodeRecord) = 
        nameGen { 
            // let! nodeId = newName ()
            let nodeId = sprintf "node%i" node.UID
            return { NodeId = nodeId;
                     NodeLabel = node.NodeLabel;
                      Shape = "box"; FillColor = None }
            }
    let genAnonNode (gridRef:WGS84Point) = 
        nameGen { 
            // Warning - this is not reliable for graphviz as 
            // we have different traversals to make nodes in 
            // the Path Tree and the Route List
            let! nodeId = newName ()
            return { NodeId = nodeId; 
                     NodeLabel = gridRef.ToString(); 
                     Shape = "box"; FillColor = None }
            }
    let genEdge (prev:GraphvizNode) (next:GraphvizNode) (edge:EdgeRecord) = 
        nameGen.Return { StartId = prev.NodeId; EndId = next.NodeId;
                         LineStyle = None; LineColour = None; 
                         EdgeLabel = Some <| makeEdgeLabel edge.DirectDistance }
    { MakeRouteNode     = genNode
    ; MakeRouteAnonNode = genAnonNode
    ; MakeRouteEdge     = genEdge }


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


type private RankDict = Dictionary<string, (int * GraphvizNode)>

/// Mutable!
let private addBestRank (node:GraphvizNode) (rank:int) (dict:RankDict) : unit =
    let nodeId = node.NodeId
    match Dictionary.tryFind nodeId dict with
    | None -> dict.Add(nodeId,(rank,node))
    | Some (rank2,_) -> 
        if rank < rank2 then
            dict.[nodeId] <- (rank,node)
        else ()

let private buildRankDict (pathTree:PathTree<GraphvizNode>) : RankDict = 
    let rankDict = new RankDict ()
    let rec work rank tree =
        match tree with
        | PathTree(node,[]) -> 
            addBestRank node rank rankDict
        | PathTree(node,kids) ->
            addBestRank node rank rankDict
            List.iter (work (rank+1)) kids
    work 1 pathTree
    rankDict

type Rank = int * GraphvizNode list

let private extractRanks (dict:RankDict) : Rank list = 
    dict.Values
        |> Seq.groupBy fst
        |> Seq.toList
        |> List.map (fun (rank,seq1) -> (rank, Seq.map snd seq1 |> Seq.toList))
        |> List.sortBy fst

let private extractRanksFromPathTree (pathTree:PathTree<GraphvizNode>) : Rank list =
    extractRanks <| buildRankDict pathTree


let genDotRanks (ranks:Rank list) : GraphvizOutput<unit> = 
    let rankProc (rank1:Rank) : GraphvizOutput<unit> = 
        anonSubgraph 
            <| graphvizOutput { 
                do! attrib <| rank "same"
                do! SL.GraphvizOutput.mapMz genDotNode (snd rank1)
                }
    SL.GraphvizOutput.mapMz rankProc ranks

let generateDot (graphName:string) (pathTree: PathTree<GraphvizNode>) (routes: Route<GraphvizNode, GraphvizEdge> list) : GraphvizOutput<unit> = 
    let ranks = extractRanksFromPathTree pathTree
    let paths = List.map edgeListFromRoute routes
    digraph graphName
            <| graphvizOutput { 
                    do! attrib <| rankdir LR
                    do! genDotRanks ranks
                    do! genDotEdges paths
                    return ()
                    }

let outputDot (graphName:string) (forest:LinkForest) (fileName:string) : Script<unit> = 
    let pathTreeDict : ExtractPathTreeDict<GraphvizNode> = 
        { MakePathTreeNode = graphvizDict.MakeRouteNode
        ; MakePathTreeAnonNode = graphvizDict.MakeRouteAnonNode } 
 
    scriptMonad { 
        let! tree       = extractPathTree pathTreeDict forest
        let! routes     = extractAllRoutes graphvizDict forest
        let gvProc      = generateDot graphName tree routes
        let gvAction    = runGraphvizOutputFile gvProc fileName
        do! (liftAction gvAction)
        }
            
