

[<AutoOpen>]
module AssetRep.SiteImport

open System.IO
open System.Data
open System.Data.SQLite

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



let testConn (conn:SQLiteConnection) : unit =  
    let query = "SELECT * FROM sites;"
    let command = new SQLiteCommand(query, conn)
    let reader : SQLiteDataReader = command.ExecuteReader()
    while reader.Read() do
        printfn "%s %s" (reader.GetString(0)) (reader.GetString(2))

let deleteData (conn:SQLiteConnection) : int =  
    let query = "DELETE FROM sites;"
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery ()



let insertDummy (conn:SQLiteConnection) = 
    let query = "INSERT INTO sites (sai_number, postal_address1, post_code) VALUES ('SAI00482900', 'YORKSTONE CLOSE', 'S2 5DN');"
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery ()


