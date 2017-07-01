[<AutoOpen>]
module AssetRep.SiteImport

open System.IO
open System.Data
open System.Data.SQLite
open FSharp.ExcelProvider

let escapeValueText (s:string) : string = 
    let escape (s1 :string) = s1.Replace("'", "''")
    match s with null -> "" | _ -> escape s



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


type SiteRow = ExcelFile< @"G:\\work\\NEXT_GEN_SURVEYS\\Supplied Info\\AI Data.xlsx",
                            SheetName = "qInstallationAddressesAndRespOf",
                            ForceString = true>


let insertRow (conn:SQLiteConnection) (site1:SiteRow.Row) : unit = 
    let query = sprintf "INSERT INTO sites (sai_number, common_name, \
                         postal_address1, postal_address2, postal_address3, postal_address4, \
                         post_code) \
                         VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s');"
                         site1.InstReference 
                         site1.InstCommonName
                         (escapeValueText <| site1.``Postal Address 1``)
                         (escapeValueText <| site1.``Postal Address 2``)
                         (escapeValueText <| site1.``Postal Address 3``)
                         (escapeValueText <| site1.``Postal Address 4``)
                         (escapeValueText <| site1.``Post Code``)
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    ignore <| command.ExecuteNonQuery ()


let insertAll (conn:SQLiteConnection) = 
    let file = new SiteRow()
    for rowi in file.Data do
        insertRow conn rowi
        printfn "%s %s" rowi.InstAssetId  rowi.InstCommonName


