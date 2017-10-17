// With Json.Net - we have low level methods if we want them


#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

let test01 () = 
    let json = JsonConvert.SerializeObject(1 : int)
    printfn "%s" json

let WriteNamed (name : string) (o : obj) (writer : JsonWriter) = 
    writer.WriteStartObject ()
    writer.WritePropertyName name
    writer.WriteValue o
    writer.WriteEndObject ()

let test02 () = 
    let json = 
        let buf = new System.Text.StringBuilder()
        let sw = new System.IO.StringWriter(buf)
        let writer = new JsonTextWriter(sw)
        WriteNamed "Age" (30 : int) writer
        buf.ToString()
    printfn "%s" json



    
