module ResultMonad


// For Seq.tail
open FSharpx.Collections

// Result is the answer type for connection monads, etc.
// We also want monadic chaining.

type Result<'a> = 
    | Ok of 'a
    | Err of string

let resultToChoice (result:Result<'a>) : Choice<string,'a> =
    match result with
    | Err(msg) -> Choice1Of2(msg)
    | Ok(a) -> Choice2Of2(a)


let inline private unitM (x:'a) : Result<'a> = Ok x


let inline private bindM (ma:Result<'a>) (f : 'a -> Result<'b>) : Result<'b> =
    match ma with
    | Ok(a) -> f a
    | Err(msg) -> Err(msg)

let fail : Result<'a> = Err "Result fail"


type ResultBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (resultMonad:ResultBuilder) = new ResultBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:Result<'a>) : Result<'b> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> Ok <| fn a

let liftM (fn:'a -> 'r) (ma:Result<'a>) : Result<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:Result<'a>) (mb:Result<'b>) : Result<'r> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> 
        match mb with 
        | Err(msg) -> Err msg
        | Ok(b) -> Ok (fn a b)

let liftM3 (fn:'a -> 'b -> 'c -> 'r) (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) : Result<'r> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> 
        match mb with 
        | Err(msg) -> Err msg
        | Ok(b) -> 
            match mc with 
            | Err(msg) -> Err msg
            | Ok(c) -> Ok (fn a b c)

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'r) (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) (md:Result<'d>) : Result<'r> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> 
        match mb with 
        | Err(msg) -> Err msg
        | Ok(b) -> 
            match mc with 
            | Err(msg) -> Err msg
            | Ok(c) -> 
                match md with
                | Err(msg) -> Err msg
                | Ok(d) -> Ok (fn a b c d)

let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'r) (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) (md:Result<'d>) (me:Result<'e>) : Result<'r> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> 
        match mb with 
        | Err(msg) -> Err msg
        | Ok(b) -> 
            match mc with 
            | Err(msg) -> Err msg
            | Ok(c) -> 
                match md with
                | Err(msg) -> Err msg
                | Ok(d) -> 
                    match me with
                    | Err(msg) -> Err msg
                    | Ok(e) -> Ok (fn a b c d e)

let tupleM2 (ma:Result<'a>) (mb:Result<'b>) : Result<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) : Result<'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) (md:Result<'d>) : Result<'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:Result<'a>) (mb:Result<'b>) (mc:Result<'c>) (md:Result<'d>) (me:Result<'e>)  : Result<'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

// NOTE - FParsec defines flipped versions of liftM* (e.g. pipe2, pipe3, ...)

let mapM (fn:'a -> Result<'b>) (xs:'a list) : Result<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> Result<'b>) : Result<'b list> = mapM fn xs

let mapMz (fn:'a -> Result<'b>) (xs:'a list) : Result<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> Result<'b>) : Result<unit> = mapMz fn xs

// TODO - it looks like this is wrong...
// Or at least causes a stack overflow.
let traverseM (fn: 'a -> Result<'b>) (source:seq<'a>) : Result<seq<'b>> = 
    let rec work (src:seq<'a>) = 
        if Seq.isEmpty src then Ok <| seq { yield! [] }
        else 
            let a = Seq.head src
            match fn a with
            | Err(msg) -> Err msg
            | Ok(a) -> 
                match work (Seq.tail src) with 
                | Err(msg) -> Err msg
                | Ok(rest) -> Ok <| seq { yield a; yield! rest }
    work source

let traverseMz (fn: 'a -> Result<'b>) (source:seq<'a>) : Result<unit> = 
    let rec work (src:seq<'a>) = 
        if Seq.isEmpty src then Ok ()
        else 
            let a = Seq.head src
            match fn a with
            | Err(msg) -> Err msg
            | Ok(a) -> 
                match work (Seq.tail src) with 
                | Err(msg) -> Err msg
                | Ok(rest) -> Ok ()
    work source

let traverseiM (fn:int -> 'a -> Result<'b>) (source:seq<'a>) : Result<seq<'b>> = 
    let rec work (ix:int) (src:seq<'a>) = 
        if Seq.isEmpty src then Ok <| seq { yield! [] }
        else 
            let a = Seq.head src
            match fn ix a with
            | Err(msg) -> Err msg
            | Ok(a) -> 
                match work (ix+1) (Seq.tail src) with 
                | Err(msg) -> Err msg
                | Ok(rest) -> Ok <| seq { yield a; yield! rest }
    work 0 source

let traverseiMz (fn:int -> 'a -> Result<'b>) (source:seq<'a>) : Result<unit> = 
    let rec work (ix:int) (src:seq<'a>) = 
        if Seq.isEmpty src then Ok ()
        else 
            let a = Seq.head src
            match fn ix a with
            | Err(msg) -> Err msg
            | Ok(a) -> 
                match work (ix+1) (Seq.tail src) with 
                | Err(msg) -> Err msg
                | Ok(rest) -> Ok ()
    work 0 source

// Applicatives (<*>)
let apM (mf:Result<'a ->'b>) (ma:Result<'a>) : Result<'b> = 
    match mf with
    | Err(msg) -> Err msg
    | Ok(fn) -> 
        match ma with
        | Err(msg) -> Err msg
        | Ok(a) -> Ok <| fn a

// Perform two actions in sequence. Ignore the results of the second action if both succeed.
let seqL (ma:Result<'a>) (mb:Result<'b>) : Result<'a> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> 
        match mb with
        | Err(msg) -> Err msg
        | Ok(_) -> Ok a

// Perform two actions in sequence. Ignore the results of the first action if both succeed.
let seqR (ma:Result<'a>) (mb:Result<'b>) : Result<'b> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(_) -> 
        match mb with
        | Err(msg) -> Err msg
        | Ok(b) -> Ok b

// Result sepcific operations
let runResult (failure: string -> 'b) (success: 'a -> 'b) (ma:Result<'a>) : 'b = 
    match ma with
    | Err(msg) -> failure msg
    | Ok(a) -> success a

let resultToOption (ma:Result<'a>) : Option<'a> = 
    match ma with
    | Err(_) -> None
    | Ok(a) -> Some a

let runResultWithError (ma:Result<'a>) : 'a = 
    match ma with
    | Err(msg) -> failwith msg
    | Ok(a) -> a


let throwError (msg:string) : Result<'a> = Err msg

let swapError (msg:string) (ma:Result<'a>) : Result<'a> = 
    match ma with
    | Err(_) -> Err msg
    | Ok(a) -> Ok a


let liftAction (action:'a) : Result<'a> = 
    try
        let ans = action
        Ok ans
    with
    | ex -> Err <| ex.ToString()

// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
let alt (ma:Result<'a>) (mb:Result<'a>) : Result<'a> = 
    match ma with
    | Err(_) -> mb
    | Ok(a) -> Ok a

// Catch failing computations, return None. 
// Successful operations are returned as Some(_).
let optional (ma:Result<'a>) : Result<'a option> = 
    match ma with
    | Err(_) -> Ok None
    | Ok(a) -> Ok <| Some a

// Perform an operation for its effect, ignore whether it succeeds or fails.
// (Comptations always return ``Ok ()``)
let optionalz (ma:Result<'a>) : Result<unit> = 
    match ma with
    | Err(_) -> Ok ()
    | Ok(_) -> Ok ()

