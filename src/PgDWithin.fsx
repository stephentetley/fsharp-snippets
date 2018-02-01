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
open Scripts.PostGIS



type SiteListTable = 
    ExcelFile< @"G:\work\Projects\events2\site-list-for-GEN.xlsx",
                SheetName = "SITE_LIST",
                ForceString = true >

type SiteListRow = SiteListTable.Row

let getSiteListRows () : SiteListRow list = 
    let dict : GetRowsDict<SiteListTable, SiteListRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SiteListTable())


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_dwithin"


let private makeDWithinINSERT (row:SiteListRow) : string option = 
    let make1 (osgb36:OSGB36Point)  = 
        sqlINSERT "spt_dwithin" 
            <|  [ stringValue       "uid"               row.``#SAINUMBER``
                ; stringValue       "name"              row.``#SITENAME``
                ; stringValue       "function_type"     row.``#ASSETTYPE``
                ; stringValue       "osgb36_ref"        row.``#GRIDREF``
                ; literalValue      "location"          <| makeSTGeogFromTextPointLiteral (osgb36ToWGS84 osgb36)
                ]

    Option.map make1 <| tryReadOSGB36Point row.``#GRIDREF``


let insertRows (rows:seq<SiteListRow>) : Script<int> = 
    let proc1 (row:SiteListRow) : PGSQLConn<int> = 
        match makeDWithinINSERT row with
        | Some sql -> execNonQuery sql
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 rows


let SetupDB(password:string) : unit = 
    let rows = 
        List.distinctBy (fun (x:SiteListRow) -> x.``#SAINUMBER``) <|  getSiteListRows ()
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A") conn 
        <|  scriptMonad { 
                let! _      = deleteAllData ()  |> logScript (sprintf "%i rows deleted")
                let! count  = insertRows rows   |> logScript (sprintf "%i rows inserted") 
                return count
                }    