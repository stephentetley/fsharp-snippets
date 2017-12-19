#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

// With Json.Net - we have low level methods if we want them

#load "JsonOutput.fs"
open JsonOutput

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
    genTokens outpath2
        |> Seq.iter (fun (t:JsonToken) -> printfn "Token: %A" t) 
    
 