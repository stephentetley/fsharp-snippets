module SL.JsonOutput

open Newtonsoft.Json

// JsonOutput Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)

type JsonOutput<'a> = JsonOutput of (JsonTextWriter -> 'a)

let inline private apply1 (ma : JsonOutput<'a>) (handle:JsonTextWriter) : 'a = 
    let (JsonOutput f) = ma in f handle

let inline private unitM (x:'a) : JsonOutput<'a> = JsonOutput (fun r -> x)


let inline private bindM (ma:JsonOutput<'a>) (f : 'a -> JsonOutput<'b>) : JsonOutput<'b> =
    JsonOutput (fun r -> let a = apply1 ma r in apply1 (f a) r)

let fail : JsonOutput<'a> = JsonOutput (fun r -> failwith "JsonOutput fail")


type JsonOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (jsonOutput:JsonOutputBuilder) = new JsonOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:JsonOutput<'a>) : JsonOutput<'b> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        let ans = apply1 ma handle in fn ans

let mapM (fn:'a -> JsonOutput<'b>) (xs:'a list) : JsonOutput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> JsonOutput<'b>) : JsonOutput<'b list> = mapM fn xs

let mapMz (fn:'a -> JsonOutput<'b>) (xs:'a list) : JsonOutput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> JsonOutput<'b>) : JsonOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> JsonOutput<'b>) (source:seq<'a>) : JsonOutput<seq<'b>> = 
    JsonOutput <| fun handle ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle) source

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> JsonOutput<'b>) (source:seq<'a>) : JsonOutput<unit> = 
    JsonOutput <| fun handle ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle in ac) 
                 () 
                 source 

// JsonOutput-specific operations
type JsonOptions = 
    { IndentLevel: int }

let runJsonOutput (opts:JsonOptions) (outputFile:string) (ma:JsonOutput<'a>) : 'a = 
    use sw : System.IO.StreamWriter = new System.IO.StreamWriter(outputFile)
    use handle : JsonTextWriter = new JsonTextWriter(sw)
    if opts.IndentLevel > 0 then
        handle.Formatting <- Formatting.Indented
        handle.Indentation <- opts.IndentLevel
    else handle.Formatting <- Formatting.None
    match ma with | JsonOutput(f) -> f handle


let tellValue (o:obj) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteValue o

// Scalars
let tellBool (value:bool) : JsonOutput<unit> = tellValue (value :> obj)

// Needs testing that this round trips with JsonExtractor
let tellDateTime (value:System.DateTime) : JsonOutput<unit> = tellValue (value :> obj)

let tellDecimal (value:decimal) : JsonOutput<unit> = tellValue (value :> obj)

let tellFloat (value:float) : JsonOutput<unit> = tellValue (value :> obj)

// Needs testing that this round trips with JsonExtractor
let tellGuid (value:System.Guid) : JsonOutput<unit> = tellValue (value :> obj)

let tellInteger (value:int) : JsonOutput<unit> = tellValue (value :> obj)

let tellInteger64 (value:int64) : JsonOutput<unit> = tellValue (value :> obj)

let tellString (value:string) : JsonOutput<unit> = 
    match value with
    | null -> tellValue ("" :> obj)
    | _ -> tellValue (value :> obj)

let tellProperty (name:string) (body:JsonOutput<unit>) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WritePropertyName name
        apply1 body handle


// An object can only contain properties, so a better interface is
// tellObject (string * JsonOutput<'a>) list : JsonOutput<unit> = 

let tellObject (body:(string * JsonOutput<unit>) list) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartObject ()
        apply1 (forMz body (fun (name,proc) -> tellProperty name proc)) handle
        handle.WriteEndObject ()
        
// A better interface would have more structure i.e. aknoledge that we have a list and
// supply the element-processing function rather than the body as a whole...
let tellArray (values:'a []) (proc1:'a -> JsonOutput<unit>) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (traverseMz proc1 values) handle
        handle.WriteEndArray ()


let tellAsArray (values:seq<'a>) (proc1:'a -> JsonOutput<unit>) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (traverseMz proc1 values) handle
        handle.WriteEndArray ()

let tellListAsArray (values:'a list) (proc1:'a -> JsonOutput<unit>) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (forMz values proc1) handle
        handle.WriteEndArray ()

