module Scripts.Grouping

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad

open Scripts.PostGIS

// Note Fsharp's groupBy is not the same as Haskell's groupBy.
// Haskell's groupBy does not do a global "collect" instead it is a linear scan,
// initializing a new group when a transition occurs.
// For "Mississippi" this means wwe have multiple groups of "i" and "s".


// Note - at the moment we only need 1 function for grouping.
// This is not enough to put in a method dictionary.
// type GroupingDict<'a,'Key> = 
//    { Projection : 'a -> 'Key }

/// This is just a record for the return type of groupBy rather than a pair.

type Grouping<'Key,'a> = 
    { GroupingKey: 'Key
      Elements: seq<'a> }

let groupingBy (projection:'a -> 'Key) (source:seq<'a>) : seq<Grouping<'Key,'a>> = 
    Seq.map (fun (a,b) -> {GroupingKey = a; Elements = b}) <| Seq.groupBy projection source

type WKText = string 

// MakeCsvRow - the int parameter is a incrementing serial number, users can ignore it
// if the wish.
type GroupingMakeHullsDict<'Key,'a> = 
    { GroupByOperation: 'a -> 'Key
      GetElementLoc: 'a -> WGS84Point option
      CsvHeaders: string list
      MakeCsvRow: int -> 'Key -> WKText -> RowWriter
    }

let private extractPoints (dict:GroupingMakeHullsDict<'Key,'a>) (source:Grouping<'Key,'a>) : seq<WGS84Point> = 
    Seq.choose id <| Seq.map (dict.GetElementLoc) source.Elements

let private concaveHull1 (dict:GroupingMakeHullsDict<'Key,'a>) (source:Grouping<'Key,'a>) (targetPercent:float) : Script<string> = 
    pgConcaveHull (Seq.toList <| extractPoints dict source) targetPercent

let private convexHull1 (dict:GroupingMakeHullsDict<'Key,'a>) (source:Grouping<'Key,'a>) : Script<string> = 
    pgConvexHull (Seq.toList <| extractPoints dict source)



let private filterPOLYGONs (source:seq<'Key * WKText>) : seq<'Key * WKText> = 
    Seq.filter (fun (a,b:WKText) -> b.Contains "POLYGON") source

type ConcaveHullOptions = 
    { TargetPercentage: float }


    // (projection:'a -> 'Key)
let private genHullsCsv (make1:Grouping<'Key,'a> -> Script<'Key * WKText>) (dict:GroupingMakeHullsDict<'Key,'a>) (source:seq<'a>) (outputFile:string) : Script<unit> =
    scriptMonad { 
        let groups = groupingBy dict.GroupByOperation source
        let! hulls = traverseM make1 groups
        let rows = 
            Seq.mapi (fun ix (key,wkt) -> dict.MakeCsvRow ix key wkt) <| filterPOLYGONs hulls
        let csvProc:CsvOutput<unit> = writeRowsWithHeaders dict.CsvHeaders rows
        do! liftAction <| outputToNew {Separator=","} csvProc outputFile
        }


let generateConcaveHullsCsv (options:ConcaveHullOptions) (dict:GroupingMakeHullsDict<'Key,'a>) (source:seq<'a>) (outputFile:string) : Script<unit> =
    let make1 (group1:Grouping<'Key,'a>) : Script<'Key * WKText> = 
        fmapM (fun x -> (group1.GroupingKey, x)) <| concaveHull1 dict group1 options.TargetPercentage
    genHullsCsv make1 dict source outputFile

let generateConvexHullsCsv (dict:GroupingMakeHullsDict<'Key,'a>) (source:seq<'a>) (outputFile:string) : Script<unit> =
    let make1 (group1:Grouping<'Key,'a>) : Script<'Key * WKText> = 
        fmapM (fun x -> (group1.GroupingKey, x)) <| convexHull1 dict group1
    genHullsCsv make1 dict source outputFile