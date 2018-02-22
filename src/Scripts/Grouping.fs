module Scripts.Grouping

open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad
open SL.PostGIS

// Note Fsharp's groupBy is not the same as Haskell's groupBy.
// Haskell's groupBy does not do a global "collect" instead it is a linear scan,
// initializing a new group when a transition occurs.
// For "Mississippi" this means we have multiple groups of "i" and "s" in Haskell.
//
// Haskell> groupBy (==) "Mississippi"
// ["M","i","ss","i","ss","i","pp","i"]
//
// F#> Seq.groupBy id "Mississippi" ;;
//  seq
//    [('M', seq ['M']); ('i', seq ['i'; 'i'; 'i'; 'i']);
//     ('s', seq ['s'; 's'; 's'; 's']); ('p', seq ['p'; 'p'])]
//
// FSharpxCollections provides List.groupNeighboursBy which has 
// the same behaviour as Haskell, but a slightly different 
// signature.


/// This is just a record for the return type of groupBy rather than a pair.
type Grouping<'Key,'a> = 
    { GroupingKey: 'Key
      Elements: seq<'a> }

      
// Design note
// The concept of groupBy is so "small" that we don't need to 
// put it into a dictionary. (At the moment...)

let groupingBy (projection:'T -> 'Key) (source:seq<'T>) : seq<Grouping<'Key,'T>> = 
    Seq.map (fun (a,b) -> {GroupingKey = a; Elements = b}) <| Seq.groupBy projection source


/// Ideally the Key should match a single item, if it matches 
/// multiple ones the sort order is unspecified.
let sortToKeyList (projection:'T -> 'Key) (source:seq<'T>) (keyList:'Key list) : seq<'T> = 
    let rec work ac keys src = 
        match src with
        | [] -> List.rev ac
        | xs -> 
            match keys with
            | [] -> 
                let front = List.rev ac in front @ xs
            | k1 :: ks -> 
                let (hits,misses) = List.partition (fun x -> k1 = projection x) xs
                work (hits @ ac) ks misses
    List.toSeq <| work [] keyList (Seq.toList source)


    