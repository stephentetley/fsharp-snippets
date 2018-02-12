﻿#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
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
#load @"SL\GraphvizOutput.fs"
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
// Table Schema: see sql/pg_pathfind_table.sql

// Note - pathplan-mock-data.csv has some extraordinary values (e.g. distances > 12km)
type PathImportTable = 
    CsvProvider< @"G:\work\Projects\events2\pathplan-mock-data.csv",
                 HasHeaders = true>

type PathImportRow = PathImportTable.Row

let getPathImportRows () : seq<PathImportRow> = 
    (new PathImportTable ()).Rows |> Seq.cast<PathImportRow>


let tryMakeEdge (row:PathImportRow) : EdgeDbRecord option = 
    let convert1 = 
        Option.bind wktPointToOSGB36 << tryReadWktPoint
    match convert1 row.StartPoint, convert1 row.EndPoint with
    | Some startPt, Some endPt -> 
        Some <| { Basetype = row.BASETYPE
                ; FunctionNode = row.FUNCTION_Link
                ; StartPoint = osgb36ToWGS84 startPt
                ; EndPoint = osgb36ToWGS84 endPt }
    | _,_ -> None

let edgeInsertDict : EdgeInsertDict<PathImportRow> = 
    { tryMakeEdgeDbRecord = tryMakeEdge }

let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let rows = getPathImportRows ()
    runConsoleScript (printfn "Success: %i modifications") conn 
        <| SetupEdgesDB edgeInsertDict rows 

// ***** Testing towards path finding...

let test01 () : unit =
    match tryReadWktPoint "POINT  ( 389330.850 501189.852) " with
    | Some (pt:WktPoint<OSGB36>) -> 
        printfn "%s => %A" (showWktPoint pt) (Option.map showOSGB36Point <| wktPointToOSGB36 pt) 
    | None -> failwith "Grr!"

let roseTree1 :PathTree<string> = 
    PathTree("A",[PathTree("B",[PathTree("C",[PathTree("D",[]); PathTree("E",[])])])])

let test02 () = 
    allRoutes roseTree1

let test03 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let startPt = 
        osgb36ToWGS84 { Easting = 389330.850<meter> ; Northing = 501189.852<meter> }

    runConsoleScript (printfn "Success: %A") conn 
        <| scriptMonad { 
            let! vs = findEdges startPt
            do! liftAction (List.iter (printfn "Edge: %A") vs)
            }


let test04 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let startPt = 
        osgb36ToWGS84 { Easting = 389330.850<meter> ; Northing = 501189.852<meter> }

    runConsoleScript (printfn "Success: %A") conn 
        <| scriptMonad { 
            let! routes = getSimpleRoutesFrom startPt
            do! liftAction (List.iter (printfn "Route: %A") routes)
            }


let test05 () = 
    generateDot <| allRoutes roseTree1