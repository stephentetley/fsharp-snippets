module Scripts.SiteOrder

open SL.AnswerMonad
open SL.Geo.WellKnownText
open SL.ScriptMonad
open SL.PostGIS

open Scripts.Grouping

// Note Scripts.Grouping now feels "not worked out".
// Potentially this module will replace it.

type LocationExtractor<'T,'point> = 'T -> 'point option

let private pointsFromGrouping (getLocation:LocationExtractor<'T,'point>) (group:Grouping<'Key,'T>) : seq<'point> = 
    Seq.choose id << Seq.map getLocation <| group.Elements

let private getCentroid1 (dict:WktCoordIso<'point,'srid>) (getLocation:LocationExtractor<'T,'point>) (group:Grouping<'Key,'T>) : Script<'point option> = 
    scriptMonad {
        let (points:seq<'point>) = pointsFromGrouping getLocation group
        let! opt = pgCentroid dict points
        return opt
        }

let getCentroids (dict:WktCoordIso<'point,'srid>) (getLocation:LocationExtractor<'T,'point>) (groups:seq<Grouping<'Key,'T>>) : Script<seq<'Key * 'point option>> = 
    let proc (group:Grouping<'Key,'T>) = 
        fmapM (fun pto -> (group.GroupingKey, pto)) <| getCentroid1 dict getLocation group
    traverseM proc groups


let siteOrder () : Script<int * 'elem> = 
    failwith "ERR"