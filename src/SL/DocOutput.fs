// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.DocOutput

open Microsoft.Office.Interop

// Word Output Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)

type WordDoc = Word.Document

type DocOutput<'a> = DocOutput of (WordDoc-> 'a)

let inline private apply1 (ma : DocOutput<'a>) (handle:WordDoc) : 'a = 
    let (DocOutput f) = ma in f handle

let inline private unitM (x:'a) : DocOutput<'a> = DocOutput (fun _ -> x)


let inline private bindM (ma:DocOutput<'a>) (f : 'a -> DocOutput<'b>) : DocOutput<'b> =
    DocOutput (fun doc -> let a = apply1 ma doc in apply1 (f a) doc)




type DocOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (docOutput:DocOutputBuilder) = new DocOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:DocOutput<'a>) : DocOutput<'b> = 
    DocOutput <| fun (doc:WordDoc) ->
        let ans = apply1 ma doc in fn ans

let mapM (fn:'a -> DocOutput<'b>) (xs:'a list) : DocOutput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> DocOutput<'b>) : DocOutput<'b list> = mapM fn xs

let mapMz (fn:'a -> DocOutput<'b>) (xs:'a list) : DocOutput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> DocOutput<'b>) : DocOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> DocOutput<'b>) (source:seq<'a>) : DocOutput<seq<'b>> = 
    DocOutput <| fun doc ->
        Seq.map (fun x -> let mf = fn x in apply1 mf doc) source

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> DocOutput<'b>) (source:seq<'a>) : DocOutput<unit> = 
    DocOutput <| fun doc ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) doc in ac) 
                 () 
                 source 
(*
// DocOutput-specific operations

let runDocOutput (ma:DocOutput<'a>) (indent:int) (outputFile:string) : 'a = 
    use sw : System.IO.StreamWriter = new System.IO.StreamWriter(outputFile)
    use handle : JsonTextWriter = new JsonTextWriter(sw)
    if indent > 0 then
        handle.Formatting <- Formatting.Indented
        handle.Indentation <- indent
    else handle.Formatting <- Formatting.None
    match ma with | DocOutput(f) -> f handle


let tellValue (o:obj) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteValue o

// Scalars
let tellBool (value:bool) : DocOutput<unit> = tellValue (value :> obj)

// Needs testing that this round trips with JsonExtractor
let tellDateTime (value:System.DateTime) : DocOutput<unit> = tellValue (value :> obj)

let tellDecimal (value:decimal) : DocOutput<unit> = tellValue (value :> obj)

let tellFloat (value:float) : DocOutput<unit> = tellValue (value :> obj)

// Needs testing that this round trips with JsonExtractor
let tellGuid (value:System.Guid) : DocOutput<unit> = tellValue (value :> obj)

let tellInteger (value:int) : DocOutput<unit> = tellValue (value :> obj)

let tellInteger64 (value:int64) : DocOutput<unit> = tellValue (value :> obj)

let tellString (value:string) : DocOutput<unit> = 
    match value with
    | null -> tellValue ("" :> obj)
    | _ -> tellValue (value :> obj)

let tellProperty (name:string) (body:DocOutput<unit>) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WritePropertyName name
        apply1 body handle


// An object can only contain properties, so a better interface is
// tellObject (string * DocOutput<'a>) list : DocOutput<unit> = 

let tellObject (body:(string * DocOutput<unit>) list) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartObject ()
        apply1 (forMz body (fun (name,proc) -> tellProperty name proc)) handle
        handle.WriteEndObject ()
        
// A better interface would have more structure i.e. aknoledge that we have a list and
// supply the element-processing function rather than the body as a whole...
let tellArray (values:'a []) (proc1:'a -> DocOutput<unit>) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (traverseMz proc1 values) handle
        handle.WriteEndArray ()


let tellAsArray (values:seq<'a>) (proc1:'a -> DocOutput<unit>) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (traverseMz proc1 values) handle
        handle.WriteEndArray ()

let tellListAsArray (values:'a list) (proc1:'a -> DocOutput<unit>) : DocOutput<unit> = 
    DocOutput <| fun (handle:JsonTextWriter) ->
        handle.WriteStartArray ()  
        apply1 (forMz values proc1) handle
        handle.WriteEndArray ()

*)