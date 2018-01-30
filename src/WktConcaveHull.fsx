#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Newtonsoft.Json.10.0.3\lib\net45"
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
open SL.AnswerMonad
open SL.PGSQLConn
open SL.Geo.Coord
open SL.JsonExtractor
open SL.JsonOutput
open SL.CsvOutput
open SL.ScriptMonad
open SL.ExcelProviderHelper

#load @"Scripts\PostGIS.fs"
#load @"Scripts\Grouping.fs"
open Scripts.PostGIS
open Scripts.Grouping


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

let concaveHullOutput (ix:int) (key:string) (wtk:WKText) : RowWriter = 
    [ tellQuotedString key
    ; tellQuotedString wtk
    ]

let test02 (pwd:string) = 
    let outputFile = @"G:\work\Projects\events2\wkt_concave_hulls_sitelist.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" pwd 
    let importRows = getImportRows ()

    let methodDict = 
        { GroupByOperation = fun (x:ImportRow) -> x.operational_contact 
          GetElementLoc = 
                fun (x:ImportRow) -> Option.map osgb36ToWGS84 <| tryReadOSGB36Point x.site_ngr
          CsvHeaders = [ "operations"; "well_known_text" ]
          MakeCsvRow = concaveHullOutput
        }
    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn 
        <| generateConcaveHullsCsv { TargetPercentage = 0.9 }
                                    methodDict
                                    importRows
                                    outputFile



// ***** OLD 
type Group<'a> = 
    { GroupName: string
      Records: 'a list }


// Generally we want the output from this script to be used by QGIS.
// Import into QGIS needs a sequential ID and a WKT column but it can have
// other columns that can provide e.g. Label



// Note - we dont really need intermediate Json.
// We just need a grouping procedure for the original tabular input, that can
// be supplied as a method dictionary.

// Structure is known!
// We have a JsonValue object which we can "tree parse".
let extractorM : JsonExtractor<Group<string> list> = 
    askArrayAsList 
        <| SL.JsonExtractor.liftM2 (fun name pts ->  { GroupName = name; Records = pts})
                                    (field "Responsibility" askString)
                                    (field "Outfalls" (askArrayAsList (field "OSGB36NGR" askString)))


let decodePoints (inputs:string list) : WGS84Point list = 
    List.choose tryReadOSGB36Point inputs |> List.map osgb36ToWGS84



let getInputs (jsonInputFile:string) : Script<Group<WGS84Point> list> = 
    fmapM (List.map (fun group -> { GroupName=group.GroupName; Records = decodePoints group.Records}))
          (liftJsonExtract extractorM jsonInputFile)



// Note - Delimited Text Layers in QGIS might only be able to show a single type of WKT element:
// i.e only POLYGONs, only MULTIPOINTs.


let pgConcaveHulls (groups:(Group<WGS84Point> list)) : Script<(int*string) list> = 
    mapiM (fun ix group1 -> 
                    fmapM (fun ans -> (ix+1,ans)) <| pgConcaveHull group1.Records 0.9) groups 


let main (pwd:string) = 
    let jsonInput = @"G:\work\Projects\events2\concave_hull_data1.json"
    let wktOutfile = @"G:\work\Projects\events2\wkt_concave_hulls1.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" pwd 
    let csvProc (oidTexts:(int*string) list) : CsvOutput<unit> = 
        writeRecordsWithHeaders ["oid"; "wkt"] 
                                oidTexts
                                (fun (a,b) -> [ tellInteger a; tellQuotedString b ])

    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn
        <| scriptMonad { 
                let! groups = getInputs jsonInput
                let! results1 = pgConcaveHulls groups
                do! liftAction <| SL.CsvOutput.outputToNew {Separator=","} (csvProc results1) wktOutfile
            }

   




///////////////////////////////////////////////////////////////////////////////

// Make input Json...


let buildImports () : (string * ImportRow list) list  =
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.operational_contact with null -> false | _ -> true
    importData.Data 
        |> Seq.filter nullPred 
        |> Seq.toList 
        |> List.groupBy (fun row -> row.operational_contact)


let genJSON (groups: (string * ImportRow list) list) : JsonOutput<unit> = 
    let tellOutfalls (outfalls : ImportRow list) : JsonOutput<unit> = 
        tellListAsArray outfalls 
                        (fun (row:ImportRow) ->
                            tellObject  [ "UID",        SL.JsonOutput.tellString <| row.asset_sai_number
                                        ; "Name",       SL.JsonOutput.tellString <| row.common_name
                                        ; "OSGB36NGR",  SL.JsonOutput.tellString <| row.site_ngr ] )
    tellAsArray groups 
                    (fun (group:(string * ImportRow list)) -> 
                        tellObject [ "Responsibility",  SL.JsonOutput.tellString   <| fst group
                                   ; "Outfalls",        tellOutfalls <| snd group ] )


let main2 () : unit = 
    let outputPath =  @"G:\work\Projects\events2\concave_hull_data1.json"
    let groups = buildImports ()
    ignore <| runJsonOutput (genJSON groups) 2 outputPath
