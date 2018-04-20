#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

#I @"..\packages\Npgsql.3.2.7\lib\net451"
#I @"..\packages\System.Threading.Tasks.Extensions.4.4.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.3\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Newtonsoft.Json.11.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

#load @"SL\AnswerMonad.fs"
#load @"SL\Tolerance.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\JsonOutput.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\PostGIS.fs"
open SL.AnswerMonad
open SL.PGSQLConn
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.JsonExtractor
open SL.JsonOutput
open SL.CsvOutput
open SL.ScriptMonad
open SL.ExcelProviderHelper
open SL.PostGIS

#load @"Scripts\Grouping.fs"
#load @"Scripts\PostGISHulls.fs"
open Scripts.Grouping
open Scripts.PostGISHulls

type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\site-list-for-hospitals.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type ImportRow = ImportTable.Row


let getImportRows () : seq<ImportRow> = 
    let dict : GetRowsDict<ImportTable, ImportRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRowsSeq dict (new ImportTable())

let test01 () = 
    groupingBy (fun (x:ImportRow) -> x.operational_contact ) <| getImportRows ()

let concaveHullOutput (ix:int) (key:string) (wtk:WellKnownText<_>) (elts:seq<ImportRow>) : RowWriter = 
    [ tellQuotedString key
    ; tellInt <| Seq.length elts
    ; tellQuotedString <| unwrapWellKnownText wtk
    ]


let hullsMethodDict:GroupingMakeHullsDict<string,ImportRow> = 
    { GroupByOperation = fun (x:ImportRow) -> x.operational_contact 
      GetElementLoc = 
            fun (x:ImportRow) -> Option.map osgb36ToWGS84 <| tryReadOSGB36Point x.site_ngr
      CsvHeaders = [ "operations"; "asset count"; "well_known_text" ]
      MakeCsvRow = concaveHullOutput
    }

let WktConcaveHulls (pwd:string) = 
    let outputFile = @"G:\work\Projects\events2\wkt_concave_hulls_sitelist.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" pwd 
    let importRows = getImportRows ()

    runConsoleScript (printfn "Success: %A") conn 
        <| generateConcaveHullsCsv { TargetPercentage = 0.9 }
                                    hullsMethodDict
                                    importRows
                                    outputFile

let WktConvexHulls (pwd:string) = 
    let outputFile = @"G:\work\Projects\events2\wkt_convex_hulls_sitelist.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" pwd 
    let importRows = getImportRows ()

    runConsoleScript (printfn "Success: %A") conn 
        <| generateConvexHullsCsv hullsMethodDict importRows outputFile



