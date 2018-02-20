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
#load @"SL\PostGIS.fs"
open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ExcelProviderHelper
open SL.ScriptMonad
open SL.PostGIS

#load @"Scripts\Grouping.fs"
#load @"Scripts\TspRouting.fs"
#load @"Scripts\SiteOrder.fs"
open Scripts.Grouping
open Scripts.TspRouting
open Scripts.SiteOrder

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

let filterOutMoreInfo (rows:seq<SiteListRow>) : seq<SiteListRow> = 
    Seq.filter (fun (row:SiteListRow) ->
                    row.``Work Center`` <> "MORE DETAILS REQUIRED") rows

let test01 () =
    let makeGrouping = fun (row:SiteListRow) -> row.``Work Center``
    let groups = groupingBy makeGrouping <| getSiteListRows ()
    Seq.iter (printfn "GROUP:\n%A") <| groups

let test02 () =
    let source = [(2,"two"); (4,"four"); (1,"one"); (3,"three")]
    let keys = [4;3;2;1]
    Seq.iter (printfn "%A") <| sortToKeyList (fst) (source |> List.toSeq) keys


type WorkGroup = string * WGS84Point option

let workGroupTspDict:TspNodeInsertDict<WorkGroup> = 
    { TryMakeNodeLocation = snd; MakeNodeLabel = fst }

let siteListRowDict : TspNodeInsertDict<SiteListRow> = 
    { TryMakeNodeLocation =
        fun (row:SiteListRow) -> 
            Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Site Grid Ref``

    ; MakeNodeLabel = 
        fun (row:SiteListRow) -> 
            sprintf "%s" row.Name }

let groupingOp (row:SiteListRow) : string =
    match row.``Work Center`` with
    | null -> "UNKNOWN WORK CENTER"
    | ss -> ss


let siteOrderDict:SiteOrderDict<string,SiteListRow> = 
    { GroupingOp = groupingOp
    ; ExtractGridRef = 
        fun (row:SiteListRow) -> 
            Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Site Grid Ref``
    ; ExtractNodeLabel = 
        fun (row:SiteListRow) -> 
            sprintf "%s" row.Name }


let main (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password

    let makeGrouping    = groupingOp
    let getGridRef      = 
        fun (row:SiteListRow) -> 
            Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Site Grid Ref``

    runConsoleScript (List.iter (fun (i,s) -> printfn "%i,%s" i s)) conn
        <| siteOrder siteOrderDict (filterOutMoreInfo <| getSiteListRows ())
            




let mainOld (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password

    let makeGrouping    = groupingOp
    let getGridRef      = 
        fun (row:SiteListRow) -> 
            Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Site Grid Ref``

    runConsoleScript (List.iteri (fun ix x -> printfn "%i,%s" (ix+1) (snd x))) conn
        <| scriptMonad { 
            
            let groups          = groupingBy makeGrouping << filterOutMoreInfo <| getSiteListRows ()
            let! groupCentroids = getCentroids wktIsoWGS84 getGridRef groups
            let! groupRoute     = tspRoute workGroupTspDict (Seq.toList groupCentroids)
            let keyList         = Seq.map snd groupRoute |> Seq.toList
            let orderedGroups   = 
                sortToKeyList (fun (x:Grouping<string,SiteListRow>) -> x.GroupingKey) groups keyList
            let! sitesInGroups  = 
                forM (Seq.toList orderedGroups) 
                     (fun og -> tspRoute siteListRowDict (Seq.toList og.Elements))
            let finalOrder      = List.concat sitesInGroups  
            return finalOrder
            }


