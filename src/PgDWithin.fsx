#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Npgsql.3.2.7\lib\net451"
#I @"..\packages\System.Threading.Tasks.Extensions.4.4.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.3\lib\net40-client"
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
open SL.Geo.Coord
open SL.AnswerMonad
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ExcelProviderHelper
open SL.ScriptMonad
open SL.PostGIS



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
    liftPGSQLConn <| deleteAllRowsRestartIdentity "spt_dwithin"


let private makeDWithinINSERT (row:SiteListRow) : string option = 
    let make1 (osgb36:OSGB36Point)  = 
        sqlINSERT "spt_dwithin" 
            <|  [ stringValue       "uid"               (row.``#SAINUMBER``.Trim())
                ; stringValue       "name"              (row.``#SITENAME``.Trim())
                ; stringValue       "function_type"     (row.``#ASSETTYPE``.Trim())
                ; stringValue       "osgb36_ref"        (row.``#GRIDREF``.Trim())
                ; literalValue      "location"          <| makeSTGeogFromTextPointLiteral (osgb36ToWGS84 osgb36)
                ]
    Option.map make1 <| tryReadOSGB36Point row.``#GRIDREF``


let insertRows (rows:seq<SiteListRow>) : Script<int> = 
    let proc1 (row:SiteListRow) : PGSQLConn<int> = 
        match makeDWithinINSERT row with
        | Some sql -> execNonQuery sql
        | None -> pgsqlConn.Return 0
    liftPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 rows


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
                
/// Objects within distance.
let makeDWithinQUERY (point:WGS84Point)  (distance:float<meter>) : string = 
    System.String.Format("""
        SELECT 
            d.uid, d.name
        FROM 
            spt_dwithin d
        WHERE
            ST_DWithin(d.location, ST_Point({0}, {1}), {2}); 
        """, point.Longitude, point.Latitude, distance)


type NeighbourRec = 
    { Uid: string
      Name: string }

let neighboursWithin (point:WGS84Point) (distance:float<meter>) : Script<NeighbourRec list> = 
    let query = makeDWithinQUERY point distance
    let procM (reader:NpgsqlDataReader) : NeighbourRec = 
        { Uid   = reader.GetString(0)
        ; Name  = reader.GetString(1) }
    liftPGSQLConn <| execReaderList query procM  

let csvHeaders = [ "Uid"; "Name"; "Neighbours within 25m"; "Neighbours within 1km" ]


let makeOutputRow (row:SiteListRow) : Script<RowWriter> =
    let removeSelf xs = List.filter (fun (x:NeighbourRec) -> x.Name <> row.``#SITENAME``) xs
    let getNeighbours (dist:float<meter>) (gridref:string) : Script<string list> = 
        match tryReadOSGB36Point row.``#GRIDREF`` with
        | None -> scriptMonad.Return []
        | Some osgb -> 
            fmapM (List.map (fun (x:NeighbourRec) -> x.Name) << removeSelf)
                <| neighboursWithin (osgb36ToWGS84 osgb) dist
    scriptMonad { 
        let! neighbours25m  = fmapM (String.concat "; ") <| getNeighbours 25.0<meter> row.``#GRIDREF``
        let! neighbours1k   = fmapM (String.concat "; ") <| getNeighbours 1000.0<meter> row.``#GRIDREF``
        return [ tellQuotedString   row.``#SAINUMBER``
               ; tellQuotedString   row.``#SITENAME``
               ; tellQuotedString   neighbours25m
               ; tellQuotedString   neighbours1k]
        }
        

let main (password:string) : unit = 
    let sites = getSiteListRows ()
    let outputFile = @"G:\work\Projects\events2\site-list-neighbours.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A") conn 
        <| scriptMonad { 
                let! rows = mapM makeOutputRow sites
                let csvProc:CsvOutput<unit> = 
                    SL.CsvOutput.writeRowsWithHeaders csvHeaders rows
                do! liftAction <| SL.CsvOutput.outputToNew {Separator=","} csvProc outputFile
                }
