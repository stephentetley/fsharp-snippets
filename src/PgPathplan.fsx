#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"


#load @"SL\AnswerMonad.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.ScriptMonad
open SL.CsvOutput

#load @"Scripts\PostGIS.fs"
#load @"Scripts\PathFinder.fs"
open Scripts.PathFinder

// PostgresSQL with PostGIS enabled.
// Table Schema:
// CREATE TABLE spt_outfalls (stc25_ref VARCHAR(12) PRIMARY KEY, function_node VARCHAR(30), osgb36_grid VARCHAR(16), point_loc geography (POINT));



type PathImportTable = 
    CsvProvider< @"G:\work\Projects\events2\pathplan-mock-data.csv",
                 HasHeaders = true>

type PathImportRow = PathImportTable.Row

let getPathImportRows () : seq<PathImportRow> = 
    (new PathImportTable ()).Rows |> Seq.cast<PathImportRow>


let tryMakeEdge (row:PathImportRow) : EdgeInsert option = 
    match tryReadWktPoint row.StartPoint, tryReadWktPoint row.EndPoint with
    | Some startPt, Some endPt -> 
        let wgs84Start = wktOSGB36ToWktWGS84 startPt
        let wgs84End = wktOSGB36ToWktWGS84 endPt 
        Some <| { Basetype = row.BASETYPE
                ; FunctionNode = row.FUNCTION_Link
                ; StartPoint = osgb36ToWGS84 <| wktToOSGB36Point startPt
                ; EndPoint = osgb36ToWGS84 <| wktToOSGB36Point endPt }
    | _,_ -> None

let edgeInsertDict : EdgeInsertDict<PathImportRow> = { tryMakeEdgeInsert = tryMakeEdge }

let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let rows = getPathImportRows ()
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| SetupEdgesDB edgeInsertDict rows 

// ***** Testing towards path finding...

let test01 () : unit =
    match tryReadWktPoint "POINT  ( 389330.850 501189.852) " with
    | Some (pt:WktPoint<OSGB36>) -> 
        printfn "%s => %s" (showWktPoint pt) (showWktPoint <| wktOSGB36ToWktWGS84 pt) 
    | None -> failwith "Grr!"

let roseTree1 :PathTree<string> = 
    PathTree("A",[PathTree("B",[PathTree("C",[PathTree("D",[]); PathTree("E",[])])])])

let test02 () = 
    allRoutes roseTree1

let test03 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let startPt = 
        osgb36ToWGS84 { Easting = 389330.850<meter> ; Northing = 501189.852<meter> }

    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn 
        <| scriptMonad { 
            let! vs = findEdges startPt
            do! liftAction (List.iter (printfn "Edge: %A") vs)
            }


let test04 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let startPt = 
        osgb36ToWGS84 { Easting = 389330.850<meter> ; Northing = 501189.852<meter> }

    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn 
        <| scriptMonad { 
            let! routes = getRoutesFrom startPt
            do! liftAction (List.iter (printfn "Route: %A") routes)
            }

