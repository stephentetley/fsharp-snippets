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
open PGSQLConn

#load "Geo.fs"
open Geo

#load "ResultMonad.fs"
#load "JsonExtractor.fs"
open JsonExtractor




let makeConnString (pwd:string) (dbname:string) : string = 
    let fmt : Printf.StringFormat<(string -> string -> string)> = "Host=localhost;Username=postgres;Password=%s;Database=%s";
    sprintf fmt pwd dbname


// TODO - all this code is just a placeholder
// Postgis can build bounding polygons so we should interface 
// with that (ST_ConvexHull)


let jsonInput = @"G:\work\Projects\pgrouting\routing_data1.json"

// Structure is known!
// We have a JsonValue object which we can "tree parse".
let extractor (jsonValue:JsonValue) : string list = 
    let extrObj (value:JsonValue) : string = value.["OSGB36NGR"].AsString()
    [ for v in jsonValue -> extrObj v ]

let readInputJson (fileName:string) : string list = 
    fileName 
        |> System.IO.File.ReadAllText
        |> JsonValue.Parse 
        |> extractor 


let extractorM : JsonExtractor<string list> = 
    jsonArrayAsList (field "OSGB36NGR" jsonString)


let readInputs (inputs:string list) : Coord.WGS84Point list = 
    let rec work (ac:Coord.WGS84Point list) (ins:string list) = 
        match ins with 
        | [] -> List.rev ac
        | (x::xs) -> 
            match Option.map (Coord.osgb36GridToWGS84) (Coord.tryReadOSGB36Grid x) with
            | None -> work ac xs
            | Some(ans) -> work (ans::ac) xs
    work [] inputs


let inputs () : Coord.WGS84Point list = 
    readInputs <| ResultMonad.runResultWithError (extractFromFile extractorM jsonInput)

let test01 () = inputs () |> List.take 14 |> Wkt.genMULTIPOINT 

let genConvexHullQuery (points:Coord.WGS84Point list) : string = 
    System.String.Format("""
        SELECT ST_AsText(ST_ConvexHull(
	        ST_Collect(
		        ST_GeomFromText('{0}')
                )) );
    """, (Wkt.genMULTIPOINT points) )

let test02 () = inputs () |> List.take 14 |> genConvexHullQuery |> printfn "%s"

let test03 (pwd:string) = 
    let connstring = makeConnString pwd "spt_geo" 
    let query = inputs () |> List.take 14 |> genConvexHullQuery 
    let proc = 
        execReaderSingleton query <| fun reader -> printfn "%s" (reader.GetString(0))
    printfn "%s" query
    runPGSQLConn proc connstring 
