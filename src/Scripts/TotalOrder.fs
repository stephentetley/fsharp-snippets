module Scripts.TotalOrder


open SL.AnswerMonad
open SL.ScriptMonad


// Need sorted input then we can compare.
// List.sortWith is equivalent to Haskell's List.sortBy


type TotalOrderDict<'a,'b,'r,'out> = 
    { CompareLeft: 'a -> 'a -> int
      CompareRight: 'b -> 'b -> int
      CompareLeftToRight: 'a -> 'b -> int
      ProcessLeft: 'a -> ScriptMonad<'r,'out> 
      ProcessRight: 'b -> ScriptMonad<'r,'out>
      ProcessBoth: 'a ->'b -> ScriptMonad<'r,'out> }


// (Not quite?) tail recursive?
let private tailRecMapM (mf:'a -> ScriptMonad<'r,'b>) (accum:'b list) (source:'a list) : ScriptMonad<'r, 'b list> =
    let rec go ac xs = 
        match xs with
        | [] -> scriptMonad.Return (List.rev ac)
        | y :: ys -> scriptMonad.Bind (mf y, fun a1 -> go (a1::ac) ys)
    go accum source

let totalOrder (dict:TotalOrderDict<'a,'b,'r,'out>) (lefts:'a list) (rights:'b list) : ScriptMonad<'r,'out list> = 
    let rec go ac xs ys = 
        match (xs,ys) with
        | xs1, [] -> tailRecMapM dict.ProcessLeft ac xs1
        | [], ys1 -> tailRecMapM dict.ProcessRight ac ys1
        | (x::xs1, y::ys1) -> 
            match dict.CompareLeftToRight x y with
            | i when i = 0 -> 
                scriptMonad.Bind (dict.ProcessBoth x y, fun a1 -> go (a1::ac) xs1 ys1)
            | i when i < 0 -> 
                scriptMonad.Bind (dict.ProcessLeft x, fun a1 -> go (a1::ac) xs1 ys)
            | i when i > 0 -> 
                scriptMonad.Bind (dict.ProcessRight y, fun a1 -> go (a1::ac) xs ys1)
            | i -> failwithf "Weird pattern failure: %i" i
    go [] (List.sortWith dict.CompareLeft lefts) (List.sortWith dict.CompareRight rights)



let totalOrderz (dict:TotalOrderDict<'a,'b,'r,'out>) (lefts:'a list) (rights:'b list) : ScriptMonad<'r,unit> = 
    let rec go xs ys = 
        match (xs,ys) with
        | xs1, [] -> mapMz dict.ProcessLeft xs1
        | [], ys1 -> mapMz dict.ProcessRight ys1
        | (x::xs1, y::ys1) -> 
            match dict.CompareLeftToRight x y with
            | i when i = 0 -> scriptMonad { 
                                let! _ = dict.ProcessBoth x y
                                do! go xs1 ys1 }
            | i when i < 0 -> scriptMonad { 
                                let! _ = dict.ProcessLeft x
                                do! go xs1 ys }
            | i when i > 0 -> scriptMonad { 
                                let! _ = dict.ProcessRight y
                                do! go xs ys1 }
            | i -> failwithf "Weird pattern failure: %i" i
    go (List.sortWith dict.CompareLeft lefts) (List.sortWith dict.CompareRight rights)

