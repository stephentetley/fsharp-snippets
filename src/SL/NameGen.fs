// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.NameGen


// For Seq.tail
open FSharpx.Collections

type private State = 
    { GenFun: int -> string
      Counter: int }

type NameGen<'a> = private NameGen of (State -> (State * 'a))


let inline private apply1 (ma:NameGen<'a>) (st:State) : (State * 'a) = 
    match ma with | NameGen fn -> fn st

let inline private returnM (x:'a) : NameGen<'a> = 
    NameGen <| fun st -> (st,x)


let inline private bindM (ma:NameGen<'a>) (f : 'a -> NameGen<'b>) : NameGen<'b> =
    NameGen <| fun st ->
        let (s1,a) = apply1 ma st in apply1 (f a) s1

/// For NameGen this is the FParsec's (>>.) combinator, or Haskell's (*>)
let inline private combineM  (ma:NameGen<unit>) (mb:NameGen<'b>) : NameGen<'b> = 
    NameGen <| fun st ->
        let (s1,_) = apply1 ma st in apply1 mb s1


type NameGenBuilder() = 
    member self.Return x        = returnM x
    member self.Bind (p,f)      = bindM p f
    member self.Zero ()         = returnM ()        // WARNING - no notion of failure
    member self.Combine (p,f)   = combineM p f

let (nameGen:NameGenBuilder) = new NameGenBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:NameGen<'a>) : NameGen<'b> = 
    NameGen <| fun st ->
        let (s1,a) = apply1 ma st in (s1, fn a)

let liftM (fn:'a -> 'r) (ma:NameGen<'a>) : NameGen<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:NameGen<'a>) (mb:NameGen<'b>) : NameGen<'r> = 
    nameGen { 
        let! a = ma
        let! b = mb
        return (fn a b)
    }

let liftM3 (fn:'a -> 'b -> 'c -> 'r) (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) : NameGen<'r> = 
    nameGen { 
        let! a = ma
        let! b = mb
        let! c = mc
        return (fn a b c)
    }

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'r) (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) (md:NameGen<'d>) : NameGen<'r> = 
    nameGen { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        return (fn a b c d)
    }


let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'r) (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) (md:NameGen<'d>) (me:NameGen<'e>) : NameGen<'r> = 
    nameGen { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        let! e = me
        return (fn a b c d e)
    }

let tupleM2 (ma:NameGen<'a>) (mb:NameGen<'b>) : NameGen<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) : NameGen<'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) (md:NameGen<'d>) : NameGen<'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:NameGen<'a>) (mb:NameGen<'b>) (mc:NameGen<'c>) (md:NameGen<'d>) (me:NameGen<'e>)  : NameGen<'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

// NOTE - FParsec defines flipped versions of liftM* (e.g. pipe2, pipe3, ...)

let mapM (fn:'a -> NameGen<'b>) (xs:'a list) : NameGen<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> returnM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a :: ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> NameGen<'b>) : NameGen<'b list> = mapM fn xs

let mapMz (fn:'a -> NameGen<'b>) (xs:'a list) : NameGen<unit> = 
    let rec work ys = 
        match ys with
        | [] -> returnM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs


let forMz (xs:'a list) (fn:'a -> NameGen<'b>) : NameGen<unit> = mapMz fn xs

let mapiM (fn:int -> 'a -> NameGen<'b>) (xs:'a list) : NameGen<'b list> = 
    let rec work ix ac ys = 
        match ys with
        | [] -> returnM <| List.rev ac
        | z :: zs -> bindM (fn ix z) (fun a -> work (ix+1) (a::ac) zs)
    work 0 [] xs


let mapiMz (fn:int -> 'a -> NameGen<'b>) (xs:'a list) : NameGen<unit> = 
    let rec work ix ys = 
        match ys with
        | [] -> returnM ()
        | z :: zs -> 
            bindM (fn ix z) (fun _ -> work (ix+1) zs)
    work 0 xs

let foriM (xs:'a list) (fn:int -> 'a -> NameGen<'b>)  : NameGen<'b list> = mapiM fn xs

let foriMz (xs:'a list) (fn:int -> 'a -> NameGen<'b>) : NameGen<unit> = mapiMz fn xs


// Note - Seq-going-through-list seems better than anything I have managed 
// directly: either with recursion (bursts the stack) or an enumerator (very slow).
// The moral is that exposing traverseM is a bad API (currently).


let traverseM (fn: 'a -> NameGen<'b>) (source:seq<'a>) : NameGen<seq<'b>> =
    fmapM (List.toSeq) (mapM fn <| Seq.toList source) 


let traverseMz (fn: 'a -> NameGen<'b>) (source:seq<'a>) : NameGen<unit> = 
    mapMz fn <| Seq.toList source

let traverseiM (fn:int -> 'a -> NameGen<'b>) (source:seq<'a>) : NameGen<seq<'b>> = 
    fmapM (List.toSeq) (mapiM fn <| Seq.toList source) 

let traverseiMz (fn:int -> 'a -> NameGen<'b>) (source:seq<'a>) : NameGen<unit> = 
    mapiMz fn <| Seq.toList source

let sequenceM (results:NameGen<'a> list) : NameGen<'a list> = 
    let rec work ac ys = 
        match ys with
        | [] -> returnM <| List.rev ac
        | ma :: zs -> bindM ma (fun a -> work  (a::ac) zs)
    work [] results

let sequenceMz (results:NameGen<'a> list) : NameGen<unit> = 
    let rec work ys = 
        match ys with
        | [] -> returnM ()
        | ma :: zs -> bindM ma (fun _ -> work zs)
    work results

// Not sure there is a case for sequenceiM

// Summing variants

let sumMapM (fn:'a -> NameGen<int>) (xs:'a list) : NameGen<int> = 
    fmapM List.sum <| mapM fn xs

let sumMapiM (fn:int -> 'a -> NameGen<int>) (xs:'a list) : NameGen<int> = 
    fmapM List.sum <| mapiM fn xs

let sumForM (xs:'a list) (fn:'a -> NameGen<int>) : NameGen<int> = 
    fmapM List.sum <| forM xs fn

let sumForiM (xs:'a list) (fn:int -> 'a -> NameGen<int>) : NameGen<int> = 
    fmapM List.sum <| foriM xs fn

let sumTraverseM (fn: 'a -> NameGen<int>) (source:seq<'a>) : NameGen<int> =
    fmapM Seq.sum <| traverseM fn source

let sumTraverseiM (fn:int -> 'a -> NameGen<int>) (source:seq<'a>) : NameGen<int> =
    fmapM Seq.sum <| traverseiM fn source

let sumSequenceM (results:NameGen<int> list) : NameGen<int> = 
    fmapM List.sum <| sequenceM results

// Applicative's (<*>)
let apM (mf:NameGen<'a ->'b>) (ma:NameGen<'a>) : NameGen<'b> = 
    NameGen <| fun st ->
        let (s1,f) = apply1 mf st
        let (s2,a) = apply1 ma s1
        (s2, f a)

// Perform two actions in sequence. Ignore the results of the second action if both succeed.
let seqL (ma:NameGen<'a>) (mb:NameGen<'b>) : NameGen<'a> = 
    NameGen <| fun st ->
        let (s1,a) = apply1 ma st
        let (s2,b) = apply1 mb s1
        (s2, a)

// Perform two actions in sequence. Ignore the results of the first action if both succeed.
let seqR (ma:NameGen<'a>) (mb:NameGen<'b>) : NameGen<'b> = 
    NameGen <| fun st ->
        let (s1,a) = apply1 ma st
        let (s2,b) = apply1 mb s1
        (s2, b)

// NameGen specific operations
let runNameGen (start:int) (genName: int -> string) (ma:NameGen<'a>) : 'a = 
    snd <| apply1 ma { GenFun = genName; Counter = start}


// Run a NameGen computation, the fresh name counter starts at 0.
let runNameGenZero (genName: int -> string) (ma:NameGen<'a>) : 'a = 
    snd <| apply1 ma { GenFun = genName; Counter = 0 }

// Run a NameGen computation, the fresh name counter starts at 1.
let runNameGenOne (genName: int -> string) (ma:NameGen<'a>) : 'a = 
    snd <| apply1 ma { GenFun = genName; Counter = 1 }

let newName () : NameGen<string> = 
    NameGen <| fun st -> let i = st.Counter in ({st with Counter = i + 1}, st.GenFun i)

let reset (start:int) (genName: int -> string) : NameGen<unit> = 
    NameGen <| fun _ -> ({ GenFun = genName; Counter = start}, ())

