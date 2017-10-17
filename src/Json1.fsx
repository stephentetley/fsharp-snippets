// With System.Runtime.Serialization.Json - this is automatic for a type


#r "System.Runtime.Serialization"

open System.Runtime.Serialization       // provides DataContract etc.
open System.Runtime.Serialization.Json


let toJson (o:'a) = 
    let serializer = new DataContractJsonSerializer(typeof<'a>)
    let encoding = System.Text.UTF8Encoding()
    use stream = new System.IO.MemoryStream()
    serializer.WriteObject(stream,o) 
    stream.Close()
    encoding.GetString(stream.ToArray())

[<DataContract>]
type Person = { 
    [<field: DataMember(Name="name")>]
    Name : string;
    [<field: DataMember(Name="age")>]
    Age : int }

let geoff = { Name = "Geoff"; Age = 65 }

let test01 () = toJson("hello" : string) |> printfn "%s"

let test02 () = toJson(geoff) |> printfn "%s"


