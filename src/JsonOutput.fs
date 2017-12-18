module JsonOutput

open Newtonsoft.Json

// JsonOutput Monad
// Output is to a handle so this is not strictly a writer monad

type JsonOutput<'a> = JsonOutput of (JsonTextWriter -> 'a)

let inline private apply1 (ma : JsonOutput<'a>) (handle:JsonTextWriter) : 'a = 
    let (JsonOutput f) = ma in f handle

let inline private unit (x:'a) : JsonOutput<'a> = JsonOutput (fun r -> x)


let inline private bind (ma:JsonOutput<'a>) (f : 'a -> JsonOutput<'b>) : JsonOutput<'b> =
    JsonOutput (fun r -> let a = apply1 ma r in apply1 (f a) r)

let fail : JsonOutput<'a> = JsonOutput (fun r -> failwith "JsonOutput fail")


type JsonOutputBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = unit ()

let (jsonOutput:JsonOutputBuilder) = new JsonOutputBuilder()

let runJsonOutput (ma:JsonOutput<'a>) (outputFile:string) : 'a = 
    use sw : System.IO.StreamWriter = new System.IO.StreamWriter(outputFile)
    use handle : JsonTextWriter = new JsonTextWriter(sw)
    match ma with | JsonOutput(f) -> f handle


//let liftConn (proc:SQLite.SQLiteConnection -> 'a) : SQLiteConn<'a> = SQLiteConn proc
    
//let execNonQuery (statement:string) : SQLiteConn<int> = 
//    SQLiteConn <| fun conn -> 
//        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
//        cmd.ExecuteNonQuery ()

let tellDict (elems:(string*obj) list) : JsonOutput<unit> = 
    JsonOutput <| fun (handle:JsonTextWriter) -> 
        let write1 (name:string) (o:obj) : unit = 
            handle.WritePropertyName name
            handle.WriteValue o
        handle.WriteStartObject ()
        List.iter (fun (k,v) -> write1 k v) elems    
        handle.WriteEndObject ()
        
let mapM (fn:'a -> JsonOutput<'b>) (xs:'a list) : JsonOutput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unit <| List.rev ac
        | z :: zs -> bind (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> JsonOutput<'b>) : JsonOutput<'b list> = mapM fn xs

let mapMz (fn:'a -> JsonOutput<'b>) (xs:'a list) : JsonOutput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unit ()
        | z :: zs -> bind (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> JsonOutput<'b>) : JsonOutput<unit> = mapMz fn xs


