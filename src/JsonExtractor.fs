module JsonExtractor

open FSharp.Data
open FSharp.Data.JsonExtensions
open System.IO

open ResultMonad


type JsonExtractor<'a> = private JsonExtractor of (JsonValue -> Result<'a>)



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

let liftM2 (fn:'a -> 'b -> 'r) (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) : JsonExtractor<'r> = 
    JsonExtractor <| fun r ->
        ResultMonad.liftM2 fn (apply1 ma r) (apply1 mb r)

let liftM3 (fn:'a -> 'b -> 'c -> 'r) (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) : JsonExtractor<'r> = 
    JsonExtractor <| fun r ->
        ResultMonad.liftM3 fn (apply1 ma r) (apply1 mb r) (apply1 mc r)

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'r) (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) (md:JsonExtractor<'d>) : JsonExtractor<'r> = 
    JsonExtractor <| fun r ->
        ResultMonad.liftM4 fn (apply1 ma r) (apply1 mb r) (apply1 mc r) (apply1 md r)

let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'r) (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) (md:JsonExtractor<'d>) (me:JsonExtractor<'e>) : JsonExtractor<'r> = 
    JsonExtractor <| fun r ->
        ResultMonad.liftM5 fn (apply1 ma r) (apply1 mb r) (apply1 mc r) (apply1 md r) (apply1 me r)


let tupleM2 (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) : JsonExtractor<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) : JsonExtractor<'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) (md:JsonExtractor<'d>) : JsonExtractor<'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:JsonExtractor<'a>) (mb:JsonExtractor<'b>) (mc:JsonExtractor<'c>) (md:JsonExtractor<'d>) (me:JsonExtractor<'e>)  : JsonExtractor<'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me


let apM (mf:JsonExtractor<'a -> 'b>) (ma:JsonExtractor<'a>) : JsonExtractor<'b> = 
    JsonExtractor <| fun r ->
        match apply1 mf r with
        | Err(msg) -> Err msg
        | Ok(f) ->
            match apply1 ma r with
            | Err(msg) -> Err msg
            | Ok(a) -> Ok (f a) 

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

let mapiM (fn:int -> 'a -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn ix y) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let mapiMz (fn:int -> 'a -> JsonExtractor<'b>) (xs: 'a list) : JsonExtractor<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn ix y) (fun _ -> work (ix+1) ys)
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
let askBool : JsonExtractor<bool> = liftJson <| fun r -> r.AsBoolean()
let askDateTime : JsonExtractor<System.DateTime> = liftJson <| fun r -> r.AsDateTime()
let askDecimal : JsonExtractor<decimal> = liftJson <| fun r -> r.AsDecimal()
let askFloat : JsonExtractor<float> = liftJson <| fun r -> r.AsFloat()
let askGuid : JsonExtractor<System.Guid> = liftJson <| fun r -> r.AsGuid()
let askInteger : JsonExtractor<int> = liftJson <| fun r -> r.AsInteger()
let askInteger64 : JsonExtractor<int64> = liftJson <| fun r -> r.AsInteger64()
let askString : JsonExtractor<string> = liftJson <| fun r -> r.AsString() 


let askArrayOf (ma:JsonExtractor<'a>) : JsonExtractor<'a []> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.fmapM Seq.toArray <| ResultMonad.traverseM (fun a -> apply1 ma a) arr 


let askArrayAsList (ma:JsonExtractor<'a>) : JsonExtractor<'a list> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.fmapM Seq.toList <| ResultMonad.traverseM (fun a -> apply1 ma a) arr 

let askArrayAsSeq (ma:JsonExtractor<'a>) : JsonExtractor<seq<'a>> = 
    JsonExtractor <| fun r ->
        let arr: JsonValue [] = [| for o in r -> o |]
        ResultMonad.traverseM (fun a -> apply1 ma a) arr 
