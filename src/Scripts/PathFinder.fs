module Scripts.PathFinder

open Npgsql

open SL.AnswerMonad
open SL.PGSQLConn
open SL.ScriptMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText


// ********** SCRIPT **********
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)

// ***** Path finding

type PathTree<'a> = 
    | PathTree of 'a * PathTree<'a> list
    


type Route<'a> = Route of 'a list




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

type Vertex =
    { UID: int
      Basetype: string
      FunctionNode: string
      StartPoint: WGS84Point
      EndPoint: WGS84Point }


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


let buildForest (startPt:WGS84Point) : Script<PathTree<Vertex> list> = 
    let rec recBuild (pt:WGS84Point) (visited:Vertex list) : Script<PathTree<Vertex> list> = 
        scriptMonad { 
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
    List.map (fun xs -> Route <| List.rev xs) <| build [] allPaths


