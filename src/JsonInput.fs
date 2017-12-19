module JsonInput

open FSharp.Core
open Newtonsoft.Json

// JsonInput Monad
// This is to help reading when we have input that is not
// directly serialized from our own objects.


// TODO - we can use a state monad with a token stream like Parsec
// to get lookahead. 
// We should only ever need 1-lookahead, but Parsec has arbitrary lookahead anyway.
type Input = seq<JsonToken>

// Tuple inside the answer rather than tuple with the answer, saves a constructor...
type Result<'a> =
    | Err of Input * string
    | Ok of Input * 'a



type JsonInput<'a> = JsonInput of (Input -> Result<'a>)

let inline private apply1 (ma : JsonInput<'a>) (input:Input) : Result<'a> = 
    let (JsonInput f) = ma in f input

let inline private unit (x:'a) : JsonInput<'a> = JsonInput (fun s -> Ok(s,x))


let inline private bind (ma:JsonInput<'a>) (f : 'a -> JsonInput<'b>) : JsonInput<'b> =
    JsonInput <| fun r -> 
        match apply1 ma r with
        | Ok(s1,a) -> apply1 (f a) s1
        | Err(s1,msg) -> Err(s1,msg)

        

let fail : JsonInput<'a> = JsonInput (fun s -> Err(s,"JsonInput fail"))


type JsonInputBuilder() = 
    member self.Return x = unit x
    member self.Bind (p,f) = bind p f
    member self.Zero () = unit ()

let (jsonInput:JsonInputBuilder) = new JsonInputBuilder()


let private tokenize (inputFile:string) : Input  = seq { 
    use sr : System.IO.StreamReader = new System.IO.StreamReader(inputFile)
    use handle : JsonTextReader = new JsonTextReader(sr)
    while handle.Read() do
    yield handle.TokenType }


let runJsonInput (ma:JsonInput<'a>) (inputFile:string) : Choice<string,'a> = 
    let input : Input  = tokenize inputFile
    match ma with 
    | JsonInput(f) -> 
        match f input with
        | Err(_,msg) -> Choice1Of2 msg
        | Ok(_,a) -> Choice2Of2 a

// There is no way to backtrack (i.e. return consumed tokens to the
// input stream), therefore we can't have a try combinator.
// Also Commas aren't tokens - so we can't scan a list for comma or end.

// TODO - it looks like we need to track the front-of-input to get lookahead.
// This is a lot of trouble, but it is the method used by the 
// "Custom JsonReader" in the Newtowsoft docs.

// Alternatively we could tokenize then parse, this would mean we can 
// strip comments easily.

(* TODO
let private askToken(token:JsonToken) : JsonInput<unit> = 
    JsonInput <| fun (handle:JsonTextReader) ->
        try
            let _ = handle.Read()
            if handle.TokenType = token then 
                Ok () 
            else Err <| sprintf "askToken - Could not match token '%A'" token
        with 
            | :? _ -> Err "askToken - out of input"

let private askPropertyName: JsonInput<string> = 
    JsonInput <| fun (handle:JsonTextReader) ->
        try
            let _ = handle.Read()
            if handle.TokenType = JsonToken.PropertyName then 
                Ok <| (handle.Value :?> string)
            else Err <| sprintf "askPropertyName - Token not a PropertyName '%A'" (handle.TokenType)
        with 
            | :? _ -> Err "askPropertyName - out of input"

// TODO - maybe this need to be of the form of Parsec's manyTill
let private nested (tokStart:JsonToken) (tokEnd:JsonToken) (body:JsonInput<'a>) : JsonInput<'a> =
    jsonInput { do!  askToken tokStart
                let! ans = body
                do!  askToken tokEnd
                return ans }

// let private nestedManyTill 

let askValue : JsonInput<obj> = 
    JsonInput <| fun (handle:JsonTextReader) ->
        if handle.Read() then
            Ok <| handle.Value
        else Err "askValue - fail"


let askObject (body:JsonInput<'a>) : JsonInput<'a> = 
    nested JsonToken.StartObject JsonToken.EndObject body
 

let askArray (body:JsonInput<'a>) : JsonInput<'a> = 
    nested JsonToken.StartArray JsonToken.EndArray body


let askSimpleProperty: JsonInput<string*obj> = 
    jsonInput { let! name = askPropertyName
                let! value = askValue
                return (name,value) }

let askProperty(body:JsonInput<'a>) : JsonInput<string*'a> = 
    jsonInput { let! name = askPropertyName
                let! value = body
                return (name,value) }
*)
(*               
// Often we have simple (string*string) pairs to write
let askSimpleDictionary (elems:(string*obj) list) : JsonInput<unit> = 
    JsonInput <| fun (handle:JsonTextReader) -> 
        let write1 (name:string) (o:obj) : unit = 
            handle.WritePropertyName name
            handle.WriteValue o
        handle.WriteStartObject ()
        List.iter (fun (k,v) -> write1 k v) elems    
        handle.WriteEndObject ()
        
let mapM (fn:'a -> JsonInput<'b>) (xs:'a list) : JsonInput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unit <| List.rev ac
        | z :: zs -> bind (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> JsonInput<'b>) : JsonInput<'b list> = mapM fn xs

let mapMz (fn:'a -> JsonInput<'b>) (xs:'a list) : JsonInput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unit ()
        | z :: zs -> bind (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> JsonInput<'b>) : JsonInput<unit> = mapMz fn xs

*)
