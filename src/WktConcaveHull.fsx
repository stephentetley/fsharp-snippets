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

#load "ResultMonad.fs"
#load "JsonExtractor.fs"
open JsonExtractor



#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json
#load "JsonOutput.fs"
open JsonOutput


#load "CsvWriter.fs"
open CsvWriter


let makeConnString (pwd:string) (dbname:string) : string = 
    let fmt : Printf.StringFormat<(string -> string -> string)> = "Host=localhost;Username=postgres;Password=%s;Database=%s";
    sprintf fmt pwd dbname


// TODO - all this code is just a placeholder
// Postgis can build bounding polygons so we should interface 
// with that (ST_ConvexHull)


let jsonInput = @"G:\work\Projects\events2\concave_hull_data1.json"

type Wgs84Multipoint = Coord.WGS84Point list
type Osgb36Multipoint  = Coord.OSGB36Grid list


type InputData = (string * Osgb36Multipoint) list




// Structure is known!
// We have a JsonValue object which we can "tree parse".


let extractorM : JsonExtractor<(string * string list) list> = 
    askArrayAsList 
        <| JsonExtractor.tupleM2 
                (field "Responsibility" askString)
                (field "Outfalls" (askArrayAsList (field "OSGB36NGR" askString)))


let readInputs (inputs:string list) : Coord.WGS84Point list = 
    let rec work (ac:Coord.WGS84Point list) (ins:string list) = 
        match ins with 
        | [] -> List.rev ac
        | (x::xs) -> 
            match Option.map (Coord.osgb36GridToWGS84) (Coord.tryReadOSGB36Grid x) with
            | None -> work ac xs
            | Some(ans) -> work (ans::ac) xs
    work [] inputs


let inputs () : (string * Coord.WGS84Point list) list = 
    ResultMonad.runResultWithError (extractFromFile extractorM jsonInput)
        |> List.map (fun (resp,gridrefs) -> (resp, readInputs gridrefs))


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


let wktOutfile = @"G:\work\Projects\events2\wkt_concave_hulls1.csv"

let main (pwd:string) = 
    let connstring = makeConnString pwd "spt_geo" 
    let groups = inputs () 
    printfn "%A" groups
    let pgProcOne (points:Coord.WGS84Point list) : PGSQLConn<string> = 
        let query = genConcaveHullQuery points 0.9
        execReaderSingleton query <| fun reader -> reader.GetString(0)

    let pgProcAll : PGSQLConn<(int*string) list> = 
        PGSQLConn.mapiM (fun (s,points) ix -> PGSQLConn.fmapM (fun ans -> (ix+1,ans)) <| pgProcOne points) groups 

    let CsvProc (oidtexts:(int*string) list) : CsvWriter<unit> = 
        csvWriter { 
            do! tellRow ["oid"; "wkt"]
            do! CsvWriter.forMz oidtexts (fun (a,b) -> tellRow [a.ToString(); quoteField b])
        }

    let (results1 :(int*string) list) = 
        match runPGSQLConn pgProcAll connstring with
        | Ok(a) -> a
        | Err(msg) -> failwithf "FATAL: %s" msg

    outputToNew (CsvProc results1) wktOutfile ","



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
    let cast1 (str:string) : obj = 
         match str with
         | null -> "" :> obj
         | _ -> str.Trim() :> obj
    let tellOutfalls (outfalls : ImportRow list) : JsonOutput<unit> = 
        tellArray  
            <| JsonOutput.forMz outfalls (fun (row:ImportRow) ->
                tellSimpleDictionary 
                    <|  [ "UID", cast1 <| row.``SAI Number``
                        ; "Name", cast1 <| row.Name
                        ; "OSGB36NGR", cast1 <| row.``Site Grid Ref`` ] )
    tellArray 
        <| JsonOutput.forMz groups (fun (group:(string * ImportRow list)) -> 
            tellObject 
                <| jsonOutput { 
                    do! tellProperty "Responsibility" (tellValue <| ((fst group) :> obj))
                    do! tellProperty "Outfalls" (tellOutfalls <| snd group)
                    } )


let main2 () : unit = 
    let outputPath =  @"G:\work\Projects\events2\concave_hull_data1.json"
    let groups = buildImports ()
    ignore <| runJsonOutput (genJSON groups) 2 outputPath
