module Scripts.PathFinder



type PathTree<'a> = 
    | PathTree of 'a * PathTree<'a> list
    


type Route<'a> = Route of 'a list

/// User supplied function to detect cycles in the path.
type SegmentEqDict<'payload> = 
    { SegmentEq : 'payload -> 'payload -> bool }

    
// A Path tree cannot have cycles (they have been identified beforehand)...
let allRoutes (allPaths:PathTree<'a>) : Route<'a> list = 
    let rec build (soFarRev:'a list) (currentTree:PathTree<'a>) : ('a list) list = 
        match currentTree with
        | PathTree(label,[]) -> [label :: soFarRev]
        | PathTree(label,paths) -> 
            List.collect (build (label::soFarRev)) paths
    List.map (fun xs -> Route <| List.rev xs) <| build [] allPaths


