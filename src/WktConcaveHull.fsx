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

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
open SL.AnswerMonad
open SL.PGSQLConn

#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
open SL.Geo

#load @"SL\JsonExtractor.fs"
open SL.JsonExtractor



#I @"..\packages\Newtonsoft.Json.10.0.3\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json
#load @"SL\JsonOutput.fs"
open SL.JsonOutput


#load @"SL\CsvOutput.fs"
open SL.CsvOutput

#load @"Scripts\PostGIS.fs"
open Scripts.PostGIS


let jsonInput = @"G:\work\Projects\events2\concave_hull_data1.json"


type Group<'a> = 
    { Name : string
      Points : 'a list }


// Structure is known!
// We have a JsonValue object which we can "tree parse".


let extractorM : JsonExtractor<Group<string> list> = 
    askArrayAsList 
        <| SL.JsonExtractor.liftM2 (fun name pts ->  { Name = name; Points = pts})
                                    (field "Responsibility" askString)
                                    (field "Outfalls" (askArrayAsList (field "OSGB36NGR" askString)))


let decodePoints (inputs:string list) : Coord.WGS84Point list = 
    List.choose Coord.tryReadOSGB36Grid inputs |> List.map Coord.osgb36GridToWGS84



let getInputs () : Answer<Group<Coord.WGS84Point> list> = 
    SL.AnswerMonad.fmapM (List.map (fun group -> { Name=group.Name; Points = decodePoints group.Points}))
                      (extractFromFile extractorM jsonInput)



// Note - Delimited Text Layers in QGIS might only be able to show a single type of WKT element:
// i.e only POLYGONs, only MULTIPOINTs.


let pgConcaveHulls (groups:(Group<Coord.WGS84Point> list)) : PGSQLConn<(int*string) list> = 
    SL.PGSQLConn.mapiM (fun ix group1 -> 
                    SL.PGSQLConn.fmapM (fun ans -> (ix+1,ans)) <| pgConcaveHull group1.Points 0.9) groups 


let wktOutfile = @"G:\work\Projects\events2\wkt_concave_hulls1.csv"


// TODO - change to Script monad...
let main (pwd:string) = 
    let conn = pgsqlConnParamsTesting "spt_geo" pwd 
    let csvProc (oidtexts:(int*string) list) : CsvOutput<unit> = 
        tellSheetWithHeaders ["oid"; "wkt"] 
                            oidtexts
                            (fun (a,b) -> [ tellInteger a; tellQuotedString b ])
    runAnswerWithError
        <| answerMonad { 
                let! groups = getInputs () 
                let! results1 = runPGSQLConn (pgConcaveHulls groups) conn
                do! liftAction (outputToNew {Separator=","} (csvProc results1) wktOutfile)
            }

   




///////////////////////////////////////////////////////////////////////////////

// Make input Json...

type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\site-list-for-hospitals.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type ImportRow = ImportTable.Row

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
