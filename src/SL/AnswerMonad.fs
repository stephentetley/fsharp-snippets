// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.AnswerMonad


// For Seq.tail
open FSharpx.Collections

// Answer is the answer type for connection monads, etc. that can fail.
// We also want monadic chaining.

type Answer<'a> = 
    | Ok of 'a
    | Err of string

let answerToChoice (result:Answer<'a>) : Choice<string,'a> =
    match result with
    | Err msg -> Choice1Of2(msg)
    | Ok a -> Choice2Of2(a)


let inline private returnM (x:'a) : Answer<'a> = Ok x


let inline private bindM (ma:Answer<'a>) (f : 'a -> Answer<'b>) : Answer<'b> =
    match ma with
    | Ok a -> f a
    | Err msg -> Err(msg)

let failM (msg:string) : Answer<'a> = Err msg

/// Both must succeed
let inline private combineM  (ma:Answer<unit>) (mb:Answer<'b>) : Answer<'b> = 
    match ma with
    | Ok () -> mb
    | Err msg -> Err msg

let inline private delayM (fn:unit -> Answer<'a>) : Answer<'a> = 
    bindM (returnM ()) fn 

let inline private sforM (source:seq<'a>) (fn: 'a -> Answer<unit>)  : Answer<unit> =
    let rec work ac ss = 
        match Seq.tryHead ss with
        | Some a -> let _ = fn a in work () (Seq.tail ss)
        | None -> Ok ()
    work () source

type AnswerBuilder() = 
    member self.Return x        = returnM x
    member self.Bind (p,f)      = bindM p f
    member self.Zero ()         = failM "Zero"
    member self.Combine (p,q)   = combineM p q
    member self.Delay fn        = delayM fn
    member self.For (s,p)       = sforM s p

    // TODO member self.ReturnFrom 

let (answerMonad:AnswerBuilder) = new AnswerBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:Answer<'a>) : Answer<'b> = 
    match ma with
    | Err msg -> Err msg
    | Ok a -> Ok <| fn a

let liftM (fn:'a -> 'r) (ma:Answer<'a>) : Answer<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:Answer<'a>) (mb:Answer<'b>) : Answer<'r> = 
    answerMonad { 
        let! a = ma
        let! b = mb
        return (fn a b)
    }

let liftM3 (fn:'a -> 'b -> 'c -> 'r) (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) : Answer<'r> = 
    answerMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        return (fn a b c)
    }

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'r) (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) (md:Answer<'d>) : Answer<'r> = 
    answerMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        return (fn a b c d)
    }

let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'r) (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) (md:Answer<'d>) (me:Answer<'e>) : Answer<'r> = 
    answerMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        let! e = me
        return (fn a b c d e)
    }

let tupleM2 (ma:Answer<'a>) (mb:Answer<'b>) : Answer<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) : Answer<'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) (md:Answer<'d>) : Answer<'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:Answer<'a>) (mb:Answer<'b>) (mc:Answer<'c>) (md:Answer<'d>) (me:Answer<'e>)  : Answer<'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

// NOTE - FParsec defines flipped versions of liftM* (e.g. pipe2, pipe3, ...)

let mapM (fn:'a -> Answer<'b>) (xs:'a list) : Answer<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> Ok <| List.rev ac
        | z :: zs -> 
            match fn z with
            | Err msg -> Err msg
            | Ok a -> work (a :: ac) zs
    work [] xs

let forM (xs:'a list) (fn:'a -> Answer<'b>) : Answer<'b list> = mapM fn xs

let mapMz (fn:'a -> Answer<'b>) (xs:'a list) : Answer<unit> = 
    let rec work ys = 
        match ys with
        | [] -> returnM ()
        | z :: zs -> 
            match fn z with
            | Err msg -> Err msg
            | Ok _ -> work zs
    work xs


let forMz (xs:'a list) (fn:'a -> Answer<'b>) : Answer<unit> = mapMz fn xs

let mapiM (fn:int -> 'a -> Answer<'b>) (xs:'a list) : Answer<'b list> = 
    let rec work ix ac ys = 
        match ys with
        | [] -> returnM <| List.rev ac
        | z :: zs -> 
            match fn ix z with
            | Err msg -> Err msg
            | Ok a -> work (ix+1) (a::ac) zs
    work 0 [] xs


let mapiMz (fn:int -> 'a -> Answer<'b>) (xs:'a list) : Answer<unit> = 
    let rec work ix ys = 
        match ys with
        | [] -> returnM ()
        | z :: zs -> 
            match fn ix z with
            | Err msg -> Err msg
            | Ok _ -> work (ix+1) zs
    work 0 xs

let foriM (xs:'a list) (fn:int -> 'a -> Answer<'b>)  : Answer<'b list> = mapiM fn xs

let foriMz (xs:'a list) (fn:int -> 'a -> Answer<'b>) : Answer<unit> = mapiMz fn xs


// Note - Seq going through list seems better than anything I can manage directly
// either with recursion (bursts the stack) or an enumerator (very slow)
// The moral is this is a abd API (currently)


let traverseM (fn: 'a -> Answer<'b>) (source:seq<'a>) : Answer<seq<'b>> =
    fmapM (List.toSeq) (mapM fn <| Seq.toList source) 


let traverseMz (fn: 'a -> Answer<'b>) (source:seq<'a>) : Answer<unit> = 
    mapMz fn <| Seq.toList source

let traverseiM (fn:int -> 'a -> Answer<'b>) (source:seq<'a>) : Answer<seq<'b>> = 
    fmapM (List.toSeq) (mapiM fn <| Seq.toList source) 

let traverseiMz (fn:int -> 'a -> Answer<'b>) (source:seq<'a>) : Answer<unit> = 
    mapiMz fn <| Seq.toList source

let sequenceM (results:Answer<'a> list) : Answer<'a list> = 
    let rec work ac ys = 
        match ys with
        | [] -> returnM <| List.rev ac
        | Err msg :: _ -> Err msg
        | Ok a :: zs -> work  (a::ac) zs
    work [] results

let sequenceMz (results:Answer<'a> list) : Answer<unit> = 
    let rec work ys = 
        match ys with
        | [] -> returnM ()
        | Err msg :: _ -> Err msg
        | Ok _ :: zs -> work zs
    work results

// Not sure there is a case for sequenceiM

// Summing variants

let sumMapM (fn:'a -> Answer<int>) (xs:'a list) : Answer<int> = 
    fmapM List.sum <| mapM fn xs

let sumMapiM (fn:int -> 'a -> Answer<int>) (xs:'a list) : Answer<int> = 
    fmapM List.sum <| mapiM fn xs

let sumForM (xs:'a list) (fn:'a -> Answer<int>) : Answer<int> = 
    fmapM List.sum <| forM xs fn

let sumForiM (xs:'a list) (fn:int -> 'a -> Answer<int>) : Answer<int> = 
    fmapM List.sum <| foriM xs fn

let sumTraverseM (fn: 'a -> Answer<int>) (source:seq<'a>) : Answer<int> =
    fmapM Seq.sum <| traverseM fn source

let sumTraverseiM (fn:int -> 'a -> Answer<int>) (source:seq<'a>) : Answer<int> =
    fmapM Seq.sum <| traverseiM fn source

let sumSequenceM (results:Answer<int> list) : Answer<int> = 
    fmapM List.sum <| sequenceM results

// Applicative's (<*>)
let apM (mf:Answer<'a ->'b>) (ma:Answer<'a>) : Answer<'b> = 
    match mf with
    | Err msg -> Err msg
    | Ok fn -> 
        match ma with
        | Err msg -> Err msg
        | Ok a -> Ok <| fn a

// Perform two actions in sequence. Ignore the results of the second action if both succeed.
let seqL (ma:Answer<'a>) (mb:Answer<'b>) : Answer<'a> = 
    match ma with
    | Err msg -> Err msg
    | Ok a -> 
        match mb with
        | Err msg -> Err msg
        | Ok _ -> Ok a

// Perform two actions in sequence. Ignore the results of the first action if both succeed.
let seqR (ma:Answer<'a>) (mb:Answer<'b>) : Answer<'b> = 
    match ma with
    | Err msg -> Err msg
    | Ok _ -> 
        match mb with
        | Err msg -> Err msg
        | Ok b -> Ok b

// Answer specific operations
let runAnswer (failure: string -> 'b) (success: 'a -> 'b) (ma:Answer<'a>) : 'b = 
    match ma with
    | Err msg -> failure msg
    | Ok a -> success a

let resultToOption (ma:Answer<'a>) : Option<'a> = 
    match ma with
    | Err _ -> None
    | Ok a -> Some a

let runAnswerWithError (ma:Answer<'a>) : 'a = 
    match ma with
    | Err msg -> failwith msg
    | Ok a -> a


let throwError (msg:string) : Answer<'a> = Err msg

let swapError (msg:string) (ma:Answer<'a>) : Answer<'a> = 
    match ma with
    | Err _ -> Err msg
    | Ok a -> Ok a


let augmentError (fn:string -> string) (ma:Answer<'a>) : Answer<'a> = 
    match ma with
    | Err msg -> Err <| fn msg
    | Ok a -> Ok a


let replaceError (defaultValue:'a) (ma:Answer<'a>) : Answer<'a> = 
    match ma with
    | Err msg -> Ok defaultValue
    | Ok a -> Ok a


let liftAction (action:'a) : Answer<'a> = 
    try
        let ans = action
        Ok ans
    with
    | err -> Err <| err.Message

let liftOption (source:'a option) : Answer<'a> = 
    match source with
    | Some a -> Ok a
    | None -> Err "None from option"

// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
let alt (ma:Answer<'a>) (mb:Answer<'a>) : Answer<'a> = 
    match ma with
    | Err _ -> mb
    | Ok a -> Ok a

// Catch failing computations, return None. 
// Successful operations are returned as Some(_).
let optional (ma:Answer<'a>) : Answer<'a option> = 
    match ma with
    | Err _ -> Ok None
    | Ok a -> Ok <| Some a

// Perform an operation for its effect, ignore whether it succeeds or fails.
// (Comptations always return ``Ok ()``)
let optionalz (ma:Answer<'a>) : Answer<unit> = 
    match ma with
    | Err _ -> Ok ()
    | Ok _ -> Ok ()

