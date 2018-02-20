module Scripts.SiteOrder

open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.ScriptMonad
open SL.PostGIS

open Scripts.Grouping
open Scripts.TspRouting

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


type SiteOrderDict<'Key,'elem> = 
    { GroupingOp: 'elem -> 'Key
      ExtractGridRef: 'elem -> WGS84Point option
      ExtractNodeLabel: 'elem -> string }


let siteOrder (dict:SiteOrderDict<string,'elem>) (unordered:seq<'elem>) : Script<(int * string) list> = 
    let centroidsTspDict:TspNodeInsertDict<string * WGS84Point option> = 
        { TryMakeNodeLocation = snd; MakeNodeLabel = fst }

    let sitesTspDict:TspNodeInsertDict<'elem> =  
        { TryMakeNodeLocation = dict.ExtractGridRef
        ; MakeNodeLabel = dict.ExtractNodeLabel }

    scriptMonad { 
            
            let groups          = groupingBy dict.GroupingOp unordered
            let! groupCentroids = getCentroids wktIsoWGS84 dict.ExtractGridRef groups
            let! groupRoute     = tspRoute centroidsTspDict (Seq.toList groupCentroids)
            let keyList         = Seq.map snd groupRoute |> Seq.toList
            let orderedGroups   = 
                sortToKeyList (fun (x:Grouping<string,'elem>) -> x.GroupingKey) groups keyList
            let! sitesInGroups  = 
                forM (Seq.toList orderedGroups) 
                     (fun og -> tspRoute sitesTspDict (Seq.toList og.Elements))
            let finalOrder      = List.concat sitesInGroups  
            return finalOrder
            }