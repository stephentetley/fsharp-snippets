[<AutoOpen>]
module AssetRep.FreshDB

open System.IO
open System.Data
open System.Data.SQLite

let makeDB (loc:string) = 
    if not <| File.Exists(loc) then
        SQLiteConnection.CreateFile(loc);

let makeConn (loc:string) : Option<SQLiteConnection> = 
    if File.Exists(loc) then
        let args = sprintf "Data Source=\"%s\";Version=3;" loc
        let conn = new SQLiteConnection(args)
        // conn.Open ()
        Some(conn)
    else 
        printfn "Database file not found '%s'" loc
        None

let withOpenConn (conn:SQLiteConnection) (fn : SQLiteConnection -> 'a) : 'a = 
    conn.Open ()
    let ans = fn conn
    conn.Close ()
    ans

type Column = { Name:string; DataType:string}

let makeColumns (xs:List<string*string>) : Column list = List.map (fun (x,y) -> {Name=x; DataType=y}) xs
    


let makeTable (name:string) (pk:Column) (others:Column list) = 
    let colpk = sprintf "%s %s PRIMARY KET NOT NULL UNIQUE" pk.Name pk.DataType
    let cols = String.concat ",\n    " <| colpk :: List.map (fun x -> sprintf "%s %s" x.Name x.DataType) others
    let sql = sprintf "CREATE TABLE %s (%s);" name cols
    sql

