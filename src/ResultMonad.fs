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

let (resultBuilder:ResultBuilder) = new ResultBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:Result<'a>) : Result<'b> = 
    match ma with
    | Err(msg) -> Err msg
    | Ok(a) -> Ok <| fn a

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


// Applicatives (<*>)
let apM (mf:Result<'a ->'b>) (ma:Result<'a>) : Result<'b> = 
    match mf with
    | Err(msg) -> Err msg
    | Ok(fn) -> 
        match ma with
        | Err(msg) -> Err msg
        | Ok(a) -> Ok <| fn a



// Result sepcific operations

let throwError (msg:string) : Result<'a> = Err msg

let swapError (msg:string) (ma:Result<'a>) : Result<'a> = 
    match ma with
    | Err(_) -> Err msg
    | Ok(a) -> Ok a

