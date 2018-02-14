#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
open FSharpx.Collections

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"


#load @"SL\AnswerMonad.fs"
#load @"SL\NameGen.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\GraphvizOutput.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.CsvOutput
open SL.GraphvizOutput
open SL.ScriptMonad


#load @"Scripts\PostGIS.fs"
#load @"Scripts\PathFinder.fs"
open Scripts.PathFinder

// PostgresSQL with PostGIS enabled.
// Table Schema: see sql/pg_pathfind_tables.sql

// Read Node data
type NodeImportTable = 
    CsvProvider< @"G:\work\Projects\events2\pathfind\pathfind-mock-nodes.csv",
                 HasHeaders = true>

type NodeImportRow = NodeImportTable.Row

let getNodeImportRows () : seq<NodeImportRow> = 
    (new NodeImportTable ()).Rows |> Seq.cast<NodeImportRow>

let tryMakeNode (row:NodeImportRow) : UserLandNode option = 
    let convert1 : string -> WGS84Point option = 
        Option.bind wktPointToWGS84 << tryReadWktPoint
    match convert1 row.WKT with
    | Some gridRef -> 
        Some <| { TypeTag       = row.``Feature Type``
                ; NodeLabel     = row.``Feature Name``
                ; NodeLocation  = gridRef }
    | _ -> None


// Read Edge data
type EdgeImportTable = 
    CsvProvider< @"G:\work\Projects\events2\pathfind\pathfind-mock-edges.csv",
                 HasHeaders = true>

type EdgeImportRow = EdgeImportTable.Row

let getEdgeImportRows () : seq<EdgeImportRow> = 
    (new EdgeImportTable ()).Rows |> Seq.cast<EdgeImportRow>

// Input data is WGS84
let tryMakeEdge (row:EdgeImportRow) : UserLandEdge option = 
    let convert1 : string -> WGS84Point option = 
        Option.bind wktPointToWGS84 << tryReadWktPoint
    match convert1 row.``Start Point (WKT)``, convert1 row.``End Point (WKT)`` with
    | Some startPt, Some endPt -> 
        Some <| { TypeTag       = row.Type
                ; EdgeLabel     = row.Description
                ; EdgeStart     = startPt
                ; EdgeEnd       = endPt }
    | _,_ -> None

let pathFindInsertDict : PathFindInsertDict<NodeImportRow,EdgeImportRow> = 
    { tryMakeUserLandNode = tryMakeNode
      tryMakeUserLandEdge = tryMakeEdge }

let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let nodeRows = getNodeImportRows ()
    let edgeRows = getEdgeImportRows ()
    runConsoleScript (printfn "Success: %i modifications") conn 
        <| SetupPathsDB pathFindInsertDict nodeRows edgeRows 

// ***** Testing towards path finding...

let test01 () : unit =
    match tryReadWktPoint "POINT  ( 389330.850 501189.852) " with
    | Some (pt:WktPoint<OSGB36>) -> 
        printfn "%s => %A" (showWktPoint pt) (Option.map showOSGB36Point <| wktPointToOSGB36 pt) 
    | None -> failwith "Grr!"

let roseTree1 : PathTree<string> = 
    PathTree("A",[PathTree("B",[PathTree("C",[PathTree("D",[]); PathTree("E",[])])])])

let test02 () = 
    List.transpose  [ ['h';'e';'l';'l';'o']
                    ; ['w';'o';'r';'l';'d']
                    ; ['!']
                    ]

let test03 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let startPt = 
        osgb36ToWGS84 { Easting = 389330.850<meter> ; Northing = 501189.852<meter> }

    runConsoleScript (printfn "Success: %A") conn 
        <| scriptMonad { 
            let! vs = findOutwardEdges startPt
            do! liftAction (List.iter (printfn "Edge: %A") vs)
            }


let test04 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A") conn 
        <| scriptMonad { 
            let! startNode = findNode "Station" "Bradford Forster Square"
            let! forest = buildForest startNode.GridRef
            let! routes = extractAllRoutes graphvizDict forest
            let procM  = generateDot "plan" routes
            do! liftAction (List.iter (printfn "Route: %A") routes)
            do! liftAction (runGraphvizOutputFile procM @"G:\work\working\output1.dot")
            }


//let test05 () = 
//    let dict:MakeEdgeDict<string,GraphvizEdge> = 
//        { MakeEdgeFromRouteNodes = 
//            fun n1 n2 -> { StartId = n1; 
//                            EndId = n2; 
//                            LineStyle = None;
//                            LineColour = Some "red1"; 
//                            EdgeLabel= None }
//        }
//    let change (route1:RouteOld<string>) : EdgeList<GraphvizEdge> = routeToEdgeList dict route1
//    let procM = generateDot "plan" << List.map change <| allRoutesTreeOld roseTree1
//    execGraphvizOutput procM
