#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

#load "Geo.fs"
open Geo

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load "ResultMonad.fs"
#load "JsonExtractor.fs"
open JsonExtractor

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
    jsonArrayOf (field "OSGB36NGR" jsonString) |> fmapM Array.toList

let inputs () : string list = 
    ResultMonad.runResultWithError <| extractFromFile extractorM jsonInput