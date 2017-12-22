module JsonOrderedInput


open FSharpx.Collections
open Newtonsoft.Json

// ***** OBSOLETE *****
// For "unpacking" Json when yoiu can't use automatic deserialization
// go with FSharp.Data and JsonValue.Parse(..).
//
// This gives us an opaque Json tree, but we can navigate it 
// (tree parsing).

// JsonInput Monad
// This demands ordered input (so object parser can work).
// Ordered input is not guaranteed by Json.



type Token = 
    { tokenType: JsonToken
      tokenValue: obj }

type Input = seq<Token>

// Tuple inside the answer rather than tuple with the answer, saves a constructor...
type Result<'a> =
    | Err of Input * string
    | Ok of Input * 'a



type JsonInput<'a> = JsonInput of (Input -> Result<'a>)



let inline private apply1 (ma : JsonInput<'a>) (input:Input) : Result<'a> = 
    let (JsonInput f) = ma in f input

let inline private unitM (x:'a) : JsonInput<'a> = JsonInput (fun s -> Ok(s,x))


let inline private bindM (ma:JsonInput<'a>) (f : 'a -> JsonInput<'b>) : JsonInput<'b> =
    JsonInput <| fun r -> 
        match apply1 ma r with
        | Ok(s1,a) -> apply1 (f a) s1
        | Err(s1,msg) -> Err(s1,msg)

        

let failM : JsonInput<'a> = JsonInput (fun s -> Err(s,"JsonInput fail"))


type JsonInputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (jsonInput:JsonInputBuilder) = new JsonInputBuilder()


let private tokenize1 (inputFile:string) : Input  = seq { 
    use sr : System.IO.StreamReader = new System.IO.StreamReader(inputFile)
    use handle : JsonTextReader = new JsonTextReader(sr)
    while handle.Read() do
        let tokValue = 
            match handle.Value with
            | null -> () :> obj
            | x -> x
        yield {tokenType=handle.TokenType; tokenValue=tokValue} }

let tokenize (inputFile:string) : Input = 
    let test (tok:Token) : bool = 
        match tok.tokenType with
        | JsonToken.Comment -> false
        | _ -> true
    Seq.filter test <| tokenize1 inputFile


let runJsonInput (ma:JsonInput<'a>) (inputFile:string) : Choice<string,'a> = 
    match (apply1 ma <| tokenize inputFile) with
        | Err(_,msg) -> Choice1Of2 msg
        | Ok(_,a) -> Choice2Of2 a

let private askToken(token:JsonToken) : JsonInput<Token> = 
    JsonInput <| fun (input:Input) ->
        if Seq.isEmpty input then
            Err(input,"askToken - out of input")
        else
            let hd = Seq.head input
            if hd.tokenType = token then
                Ok(Seq.tail input, hd)
            else
                let msg = sprintf "askToken - wrong token type wanted:'%A', saw:'%A'"
                                  token
                                  hd.tokenType
                Err(input, msg)

let private askTokenAny : JsonInput<Token> = 
    JsonInput <| fun (input:Input) ->
        if Seq.isEmpty input then
            Err(input,"askToken - out of input")
        else
            let hd = Seq.head input
            Ok(Seq.tail input, hd)


let private peekToken : JsonInput<Token> = 
    JsonInput <| fun (input:Input) ->
        if Seq.isEmpty input then
            Err(input,"askToken - out of input")
        else
            Ok(input, Seq.head input)


let askProperty: JsonInput<string*obj> = 
    jsonInput { let! nameTok = askToken JsonToken.PropertyName
                let! valueTok = askTokenAny 
                return (nameTok.tokenValue :?> string, valueTok.tokenValue) }

let askProperty1 (valueP:JsonInput<'a>) : JsonInput<string*'a> = 
    jsonInput { let! nameTok = askToken JsonToken.PropertyName
                let! valueObj = valueP 
                return (nameTok.tokenValue :?> string, valueObj) }

// This assumes the supplied body parser parsers all the fields of
// the object.
let askObject (bodyP:JsonInput<'a>): JsonInput<'a> = 
    jsonInput { let! _ = askToken JsonToken.StartObject
                let! ans = bodyP
                let! _ = askToken JsonToken.EndObject
                return ans }

// This is suffers from a horrible slow down, logging progress
let askArray (elementP:JsonInput<'a>): JsonInput<'a list> = 
    let mutable i = 1
    let rec work (ac:'a list)  = 
        printfn "work %i >>>" i 
        i <- i + 1
        bindM peekToken <| fun (tok:Token) -> 
            match tok.tokenType with
            | JsonToken.EndArray -> 
                // need to consume...
                bindM askTokenAny (fun _ -> unitM (List.rev ac))
            | _ -> bindM elementP (fun o -> work (o::ac))
    jsonInput { let! _ = askToken JsonToken.StartArray
                let! ans = work []
                return ans }



let fmapM (fn:'a -> 'b) (ma:JsonInput<'a>) : JsonInput<'b> = 
    JsonInput <| fun input -> 
        match apply1 ma input with
        | Err(s1,msg) -> Err(s1,msg) 
        | Ok(s1,ans) -> Ok (s1, fn ans)

let askStringValue : JsonInput<string> = 
    fmapM (fun (o:Token) -> o.tokenValue :?> string)
        <| askToken JsonToken.String 

let askStringDictionary : JsonInput<(string*string) list> = 
    let pairP : JsonInput<(string*string)> = askProperty1 askStringValue
    let rec work (ac:(string*string) list) = 
        bindM peekToken <| fun (tok:Token) -> 
            match tok.tokenType with
            | JsonToken.EndObject -> 
                // need to consume...
                bindM askTokenAny (fun _ -> unitM (List.rev ac)) 
            | _ -> bindM pairP (fun o -> work (o::ac))
    jsonInput { let! _ = askToken JsonToken.StartObject
                let! ans = work []
                return ans }

let mapM (fn:'a -> JsonInput<'b>) (xs:'a list) : JsonInput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> JsonInput<'b>) : JsonInput<'b list> = mapM fn xs

let mapMz (fn:'a -> JsonInput<'b>) (xs:'a list) : JsonInput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> JsonInput<'b>) : JsonInput<unit> = mapMz fn xs

