module JsonExtractor

open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO

open ResultMonad


type JsonExtractor<'a> = 
    | JsonExtractor of (JsonValue -> Result<'a>)



let inline private apply1 (ma : JsonExtractor<'a>) (jsVal:JsonValue)  : Result<'a> = 
    let (JsonExtractor fn) = ma  in  fn jsVal

let private unitM (x:'a) : JsonExtractor<'a> = 
    JsonExtractor <| fun r -> Ok x

let private bindM (ma:JsonExtractor<'a>) (f : 'a -> JsonExtractor<'b>) : JsonExtractor<'b> =
    JsonExtractor <| fun r -> 
        match apply1 ma r with
        | Err(msg) -> Err msg
        | Ok(a) -> apply1 (f a) r

let fail : JsonExtractor<'a> = 
    JsonExtractor (fun _ -> Err "JsonExtractor fail")

type JsonExtractorBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let jsonExtractor:JsonExtractorBuilder = new JsonExtractorBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:JsonExtractor<'a>) : JsonExtractor<'b> = 
    JsonExtractor <| fun r ->
        match apply1 ma r with
        | Err(msg) -> Err msg
        | Ok(a) -> Ok <| fn a


let mapM (fn: 'a -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> JsonExtractor<'b>) : JsonExtractor<'b list> = mapM fn xs


let mapMz (fn: 'a -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> JsonExtractor<'b>) : JsonExtractor<unit> = mapMz fn xs

let traverseM (fn: 'a -> JsonExtractor<'b>) (source:seq<'a>) : JsonExtractor<seq<'b>> = 
    JsonExtractor <| fun r ->
        ResultMonad.traverseM (fun a -> apply1 (fn a) r) source


let traverseMz (fn: 'a -> JsonExtractor<'b>) (source:seq<'a>) : JsonExtractor<unit> = 
    JsonExtractor <| fun r ->
        ResultMonad.traverseMz (fun a -> apply1 (fn a) r) source

let mapiM (fn: 'a -> int -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let mapiMz (fn: 'a -> int -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

// JsonExtractor-specific operations


let extractFromFile (ma:JsonExtractor<'a>) (fileName:string) : Result<'a> =
    fileName 
        |> System.IO.File.ReadAllText
        |> JsonValue.Parse 
        |> apply1 ma

let askM : JsonExtractor<JsonValue> = JsonExtractor <| Ok

let liftJson (proc: JsonValue -> 'a) : JsonExtractor<'a> = 
    JsonExtractor <| fun r -> 
        try 
            let ans = proc r in Ok ans
        with
        | ex -> Err <| ex.ToString()
    

let field (name:string) (ma:JsonExtractor<'a>) : JsonExtractor<'a> =
    JsonExtractor <| fun r -> 
        try
            let ans = r.[name] in apply1 ma ans
        with
        | ex -> Err <| ex.ToString()



// Scalars
let jsonBool : JsonExtractor<bool> = liftJson <| fun r -> r.AsBoolean()
let jsonDateTime : JsonExtractor<System.DateTime> = liftJson <| fun r -> r.AsDateTime()
let jsonDecimal : JsonExtractor<decimal> = liftJson <| fun r -> r.AsDecimal()
let jsonFloat : JsonExtractor<float> = liftJson <| fun r -> r.AsFloat()
let jsonGuid : JsonExtractor<System.Guid> = liftJson <| fun r -> r.AsGuid()
let jsonInteger : JsonExtractor<int> = liftJson <| fun r -> r.AsInteger()
let jsonInteger64 : JsonExtractor<int64> = liftJson <| fun r -> r.AsInteger64()
let jsonString : JsonExtractor<string> = liftJson <| fun r -> r.AsString() 


let jsonArrayOf (ma:JsonExtractor<'a>) : JsonExtractor<'a []> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.fmapM Seq.toArray <| ResultMonad.traverseM (fun a -> apply1 ma a) arr 


let jsonArrayAsList (ma:JsonExtractor<'a>) : JsonExtractor<'a list> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.fmapM Seq.toList <| ResultMonad.traverseM (fun a -> apply1 ma a) arr 

let jsonArrayAsSeq (ma:JsonExtractor<'a>) : JsonExtractor<seq<'a>> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.traverseM (fun a -> apply1 ma a) arr 
