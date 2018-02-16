#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\AnswerMonad.fs"
#load @"SL\Coord.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\ScriptMonad.fs"
open SL.Geo.Coord
open SL.AnswerMonad
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ExcelProviderHelper
open SL.ScriptMonad

#load @"Scripts\PostGIS.fs"
#load @"Scripts\Grouping.fs"
open Scripts.PostGIS
open Scripts.Grouping


type SiteListTable = 
    ExcelFile< @"G:\work\Projects\events2\EDM2 Site-List.xlsx",
                SheetName = "SITE_LIST",
                ForceString = true >

type SiteListRow = SiteListTable.Row

let getSiteListRows () : seq<SiteListRow> = 
    let dict : GetRowsDict<SiteListTable, SiteListRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRowsSeq dict (new SiteListTable())

//let groupByDict:GeoGroupByDict<string,SiteListRow> = 
//    { GroupBy = 
//        fun (row:SiteListRow) -> row.``Work Center``
//    ; ElementGridRef = 
//        fun (row:SiteListRow) -> 
//            Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Site Grid Ref`` }

let test01 () =
    let makeGrouping = fun (row:SiteListRow) -> row.``Work Center``
    let groups = groupingBy makeGrouping <| getSiteListRows ()
    Seq.iter (printfn "GROUP:\n%A") <| groups




// Seq vs List...
// let myMap (fn:'a -> 'b) (source:seq<'a>) : seq<'b> = Seq.map fn source
// Below doesn't typecheck because list2 is actually seq<int>
// Seq.map returns a seq<> when applied to a list

// let test02 () =
//    let (list1:int list) = [1;2;3;4]
//    let (list2:int list) = myMap (fun a -> a+10) list1
//    printfn "%A" <| list2
