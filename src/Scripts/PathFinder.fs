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
// TODO - This is and edge not a vertex!
type VertexInsert =
    { Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }


type VertexInsertDict<'inputrow> = 
    { tryMakeVertexInsert : 'inputrow -> VertexInsert option }


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_pathfind"



let private makeVertexINSERT (vertex1:VertexInsert) : string = 
    let makePointLit (pt:WGS84Point) : string = 
        sprintf "ST_GeogFromText('SRID=4326;%s')" (showWktPoint <| wgs84PointToWKT pt)
    // Note the id column is PG's SERIAL type so it is inserted automatically
    sqlINSERT "spt_pathfind" 
            <|  [ stringValue       "basetype"          vertex1.Basetype
                ; stringValue       "function_node"     vertex1.FunctionNode
                ; literalValue      "start_point"       <| makePointLit vertex1.StartPoint
                ; literalValue      "end_point"         <| makePointLit vertex1.EndPoint
                ]

let insertOutfalls (dict:VertexInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    let proc1 (row:'inputrow) : PGSQLConn<int> = 
        match dict.tryMakeVertexInsert row with
        | Some vertex -> execNonQuery <| makeVertexINSERT vertex
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 outfalls

let SetupVertexDB (dict:VertexInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    scriptMonad { 
        let! _ = deleteAllData () |> logScript (sprintf "%i rows deleted")
        let! count = insertOutfalls dict outfalls  |> logScript (sprintf "%i rows inserted") 
        return count
     }


// ***** Path finding

type PathTree<'a> = 
    | PathTree of 'a * PathTree<'a> list
    


type Route<'a> = Route of 'a list


type Vertex =
    { UID: int
      Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }

// SELECT id, basetype, function_node, ST_AsText(end_point) 
// FROM spt_pathfind 
// WHERE start_point = ST_GeomFromText('POINT(-2.16438 54.40591 )', 4326);
let makeFindVerticesQUERY (startPt:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            id, basetype, function_node, ST_AsText(end_point)
        FROM 
            spt_pathfind
        WHERE 
            start_point = ST_GeomFromText('{0}', 4326);
        """, showWktPoint <| wgs84PointToWKT startPt)




let findVertices (startPt:WGS84Point) : Script<Vertex list> = 
    let query = makeFindVerticesQUERY startPt
    let procM (reader:NpgsqlDataReader) : Vertex = 
        let wgs84End = 
            match tryReadWktPoint (reader.GetString(3)) with
            | Some pt -> wktToWGS84Point pt
            | None -> failwith "findVertices ..."
        { UID           = int <| reader.GetInt32(0)
        ; Basetype      = reader.GetString(1)
        ; FunctionNode  = reader.GetString(2)
        ; StartPoint    = startPt
        ; EndPoint      = wgs84End }
    liftWithConnParams << runPGSQLConn <| execReaderList query procM  

let notVisited (visited:Vertex list) (v1:Vertex) = 
    not <| List.exists (fun (v:Vertex) -> v.UID = v1.UID) visited

/// A start-point may have many outward paths, hence we build a list of trees.
/// Note - if we study the data we should be able to prune the searches 
/// by looking at Function_link and only following paths that start with 
/// a particular link type.
let buildForest (startPt:WGS84Point) : Script<PathTree<Vertex> list> = 
    let rec recBuild (pt:WGS84Point) (visited:Vertex list) : Script<PathTree<Vertex> list> = 
        scriptMonad { 
            // TO CHECK - Are we sure we are handling cyclic paths "wisely"?
            let! (vsNew:Vertex list) = fmapM (List.filter (notVisited visited)) <| findVertices pt
            let! branches = 
                forM vsNew <| 
                    fun (v1:Vertex) -> 
                        scriptMonad { 
                            let! kids = recBuild v1.EndPoint (v1::visited)
                            return (PathTree(v1,kids))
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

let getRoutesFrom (startPt:WGS84Point) : Script<Route<Vertex> list> = 
    fmapM (List.collect allRoutes) <| buildForest startPt




