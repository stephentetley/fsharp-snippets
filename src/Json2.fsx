#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

open FSharp.Core

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
open FSharpx.Collections

// With Json.Net - we have low level methods if we want them

#load "JsonOutput.fs"
open JsonOutput
#load "JsonOrderedInput.fs"
open JsonOrderedInput


let test01 () = 
    let json = JsonConvert.SerializeObject(1 : int)
    printfn "%s" json

let WriteNamed (name : string) (o : obj) (writer : JsonWriter) = 
    writer.WriteStartObject ()
    writer.WritePropertyName name
    writer.WriteValue o
    writer.WriteEndObject ()

let WriteDict (elems:(string*obj) list) (writer : JsonWriter) = 
    let write1 (name:string) (o:obj) : unit = 
        writer.WritePropertyName name
        writer.WriteValue o
    writer.WriteStartObject ()
    List.iter (fun (k,v) -> write1 k v) elems    
    writer.WriteEndObject ()


let test02 () = 
    let json = 
        let buf = new System.Text.StringBuilder()
        let sw = new System.IO.StringWriter(buf)
        let writer = new JsonTextWriter(sw)
        WriteNamed "Age" (30 : int) writer
        buf.ToString()
    printfn "%s" json


let test03 () = 
    let json = 
        let buf = new System.Text.StringBuilder()
        let sw = new System.IO.StringWriter(buf)
        let writer : JsonTextWriter = new JsonTextWriter(sw)
        WriteDict [ "#NAME", "NEW HADSLEY" :> obj
                  ; "#OID", "OBJ786" :> obj
                  ; "#COUNT", 34 :> obj ]
                  writer
        buf.ToString()
    printfn "%s" json


let test04 () = 
    let outpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..",@"data\output1.json")
    use sw = new System.IO.StreamWriter(outpath)
    use writer : JsonTextWriter = new JsonTextWriter(sw)
    WriteDict [ "#NAME", "NEW HADSLEY" :> obj
                ; "#OID", "OBJ786" :> obj
                ; "#COUNT", 34 :> obj ]
                writer


let outpath2 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..",@"data\output2.json")

let test05 () = 
    let proc = 
        tellSimpleDictionary 
            <| [ "#NAME", "MALBURY" :> obj
                ; "#OID", "OBJ546" :> obj
                ; "#COUNT", 12 :> obj ]
    runJsonOutput proc 4 outpath2

// Values must be plain strings (or numbers)
let deserializeKVs (input:string) : (string*string) list = 
    JsonConvert.DeserializeObject<Map<string,string>>(input) |> Map.toList
    
let test06 () = 
    use sr = new System.IO.StreamReader(outpath2)
    let input = sr.ReadToEnd() 
    printfn "%s" input
    let jObj  = deserializeKVs input
    printfn "%A" jObj

let genTokens (path:string) : seq<JsonToken> =  
    seq { 
        use sr = new System.IO.StreamReader(outpath2)
        use jr = new JsonTextReader(sr)
        while jr.Read() do
            yield jr.TokenType
    }

let test07 () = 
    let allSubsitutions = @"G:\work\Projects\events2\survey-findreplace.json"
    genTokens allSubsitutions
        |> Seq.iter (fun (t:JsonToken) -> printfn "Token: %A" t) 
    
let fibs : seq<int>  = Seq.unfold (fun (a,b) -> let c = a+b in Some (c, (b,c))) (0,1)
// Testing to see if Seq is usable for lookahead
let test08 () = 
    let input = fibs
    let a = Seq.head input
    let b = Seq.head input
    let input2 = Seq.tail input
    let c = Seq.head input2
    printf "%i %i %i .." a b c

type Dict = Map<string,string>

let readDict (json:string) : Dict =  JsonConvert.DeserializeObject<Dict>(json)

let test09 () = 
    let json = @"{ 'name1': 'aardvark', 'name2': 'zebra' }"
    printfn "%A" <| readDict json

type Rec1 = { rec1File:string; rec1Dict:Dict }

let readRec (json:string) : Rec1 =
    let dict1 = readDict json // this doesn't work, it is <string,obj> not <string,string>
    try 
       let file = Map.find "File" dict1
       let pairs = readDict <| Map.find "Names" dict1
       { rec1File=file; rec1Dict=pairs }
    with 
        | _ -> failwith "Bad"

let testInput = @"{ 'File' : 'output.doc', 'Names' : { } }"

let test10 () = 
    printfn "%A" <| readDict testInput

type Token = 
    { tokenType: JsonToken
      tokenValue: obj }

type Input = seq<Token>

let private tokenize1 (inputFile:string) : Input  = seq { 
    use sr : System.IO.StreamReader = new System.IO.StreamReader(inputFile)
    use handle : JsonTextReader = new JsonTextReader(sr)
    while handle.Read() do
        let tokValue = 
            match handle.Value with
            | null -> () :> obj
            | x -> x
        yield {tokenType=handle.TokenType; tokenValue=tokValue} }

let test11 () =
    let allSubsitutions = @"G:\work\Projects\events2\survey-findreplace.json"
    tokenize1 allSubsitutions