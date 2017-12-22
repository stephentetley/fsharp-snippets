﻿module JsonOutput

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


// Common operations
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

// JsonOutput-specific operations

let runJsonOutput (ma:JsonOutput<'a>) (indent:int) (outputFile:string) : 'a = 
    use sw : System.IO.StreamWriter = new System.IO.StreamWriter(outputFile)
    use handle : JsonTextWriter = new JsonTextWriter(sw)
    if indent > 0 then
        handle.Formatting <- Formatting.Indented
        handle.Indentation <- indent
    else handle.Formatting <- Formatting.None
    match ma with | JsonOutput(f) -> f handle


let tellValue (o:obj) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteValue o

let tellObject (body:JsonOutput<'a>) : JsonOutput<'a> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartObject ()    
        let ans = apply1 body handle
        handle.WriteEndObject ()
        ans

let tellArray (body:JsonOutput<'a>) : JsonOutput<'a> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        let ans = apply1 body handle
        handle.WriteEndArray ()
        ans

let tellSimpleProperty (name:string) (value:obj) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WritePropertyName name
        handle.WriteValue value

let tellProperty (name:string) (body:JsonOutput<'a>) : JsonOutput<'a> = 
    JsonOutput <| fun (handle:JsonTextWriter) ->
        handle.WritePropertyName name
        apply1 body handle

// Often we have simple (string*string) pairs to write
let tellSimpleDictionary (elems:(string*obj) list) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) -> 
        let write1 (name:string) (o:obj) : unit = 
            handle.WritePropertyName name
            handle.WriteValue o
        handle.WriteStartObject ()
        List.iter (fun (k,v) -> write1 k v) elems    
        handle.WriteEndObject ()

