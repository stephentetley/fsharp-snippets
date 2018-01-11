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
#load @"ResultMonad.fs"
#load @"SqlUtils.fs"
#load @"PGSQLConn.fs"
open ResultMonad
open PGSQLConn

#load "Geo.fs"
open Geo

#load "JsonExtractor.fs"
open JsonExtractor



#I @"..\packages\Newtonsoft.Json.10.0.3\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json
#load "JsonOutput.fs"
open JsonOutput


#load "CsvOutput.fs"
open CsvOutput


let makeConnString (pwd:string) (dbname:string) : string = 
    let fmt : Printf.StringFormat<(string -> string -> string)> = 
        "Host=localhost;Username=postgres;Password=%s;Database=%s";
    sprintf fmt pwd dbname


let jsonInput = @"G:\work\Projects\events2\concave_hull_data1.json"


type Group<'a> = 
    { Name : string
      Points : 'a list }


// Structure is known!
// We have a JsonValue object which we can "tree parse".


let extractorM : JsonExtractor<Group<string> list> = 
    askArrayAsList 
        <| JsonExtractor.liftM2 (fun name pts ->  { Name = name; Points = pts})
                                (field "Responsibility" askString)
                                (field "Outfalls" (askArrayAsList (field "OSGB36NGR" askString)))


let decodePoints (inputs:string list) : Coord.WGS84Point list = 
    List.choose Coord.tryReadOSGB36Grid inputs |> List.map Coord.osgb36GridToWGS84



let getInputs () : Result<Group<Coord.WGS84Point> list> = 
    ResultMonad.fmapM (List.map (fun group -> { Name=group.Name; Points = decodePoints group.Points}))
                      (extractFromFile extractorM jsonInput)


let genConvexHullQuery (points:Coord.WGS84Point list) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConvexHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                )) );
    """, (Wkt.genMULTIPOINT points) )


// Note TargetPercent of 1.0 gives a convex hull (0.9 seems okay)
let genConcaveHullQuery (points:Coord.WGS84Point list) (targetPercent:float) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConcaveHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                ), {1}) );
    """, (Wkt.genMULTIPOINT points), targetPercent )


// Note - Delimited Text Layers in QGIS might only be able to show a single type of WKT element:
// i.e only POLYGONs, only MULTIPOINTs.

//let test02 () = inputs () |> List.take 14 |> genConvexHullQuery |> printfn "%s"
let pgConcaveHull (points:Coord.WGS84Point list) : PGSQLConn<string> = 
        let query = genConcaveHullQuery points 0.9
        execReaderSingleton query <| fun reader -> reader.GetString(0)

let pgConcaveHulls (groups:(Group<Coord.WGS84Point> list)) : PGSQLConn<(int*string) list> = 
        PGSQLConn.mapiM (fun ix group1 -> PGSQLConn.fmapM (fun ans -> (ix+1,ans)) <| pgConcaveHull group1.Points) groups 


let wktOutfile = @"G:\work\Projects\events2\wkt_concave_hulls1.csv"


// Note - main should run in the result monad...
let main (pwd:string) = 
    let conn = pgsqlConnParamsTesting pwd "spt_geo" 
    let csvProc (oidtexts:(int*string) list) : CsvOutput<unit> = 
        tellSheetWithHeaders ["oid"; "wkt"] 
                            oidtexts
                            (fun (a,b) -> [ tellInteger a; tellQuotedString b ])
    runResultWithError
        <| resultMonad { 
                let! groups = getInputs () 
                let! results1 = runPGSQLConn (pgConcaveHulls groups) conn
                do! liftAction (outputToNew (csvProc results1) wktOutfile ",")
            }

   




///////////////////////////////////////////////////////////////////////////////

// Make input Json...

type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\truncated-site-list.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type ImportRow = ImportTable.Row

let buildImports () : (string * ImportRow list) list  =
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.GetValue(0) with null -> false | _ -> true
    importData.Data 
        |> Seq.filter nullPred 
        |> Seq.toList 
        |> List.groupBy (fun row -> row.``Operational Responsibility``)


let genJSON (groups: (string * ImportRow list) list) : JsonOutput<unit> = 
    let tellOutfalls (outfalls : ImportRow list) : JsonOutput<unit> = 
        tellListAsArray outfalls 
                        (fun (row:ImportRow) ->
                            tellObject  [ "UID",        JsonOutput.tellString <| row.``SAI Number``
                                        ; "Name",       JsonOutput.tellString <| row.Name
                                        ; "OSGB36NGR",  JsonOutput.tellString <| row.``Site Grid Ref`` ] )
    tellAsArray groups 
                    (fun (group:(string * ImportRow list)) -> 
                        tellObject [ "Responsibility",  JsonOutput.tellString   <| fst group
                                   ; "Outfalls",        tellOutfalls <| snd group ] )


let main2 () : unit = 
    let outputPath =  @"G:\work\Projects\events2\concave_hull_data1.json"
    let groups = buildImports ()
    ignore <| runJsonOutput (genJSON groups) 2 outputPath
