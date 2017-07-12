[<AutoOpen>]
module AssetRep.SiteImport

open System
open System.IO
open System.Data
open System.Data.SQLite
open FSharp.ExcelProvider

let escapeValueText (s:string) : string = 
    let escape (s1 :string) = s1.Replace("'", "''")
    match s with null -> "" | _ -> escape s

let cleanseValue (s:string) : string = s.TrimEnd () |> escapeValueText


let makeConn (loc:string) : Option<SQLiteConnection> = 
    if File.Exists(loc) then
        let args = sprintf "Data Source=\"%s\";Version=3;" loc
        let conn = new SQLiteConnection(args)
        Some(conn)
    else 
        printfn "Database file not found '%s'" loc
        None

let withOpenConn (conn:SQLiteConnection) (fn : SQLiteConnection -> 'a) : 'a = 
    conn.Open ()
    let ans = fn conn
    conn.Close ()
    ans

let runNonQuery (conn:SQLiteConnection) (statement:string) : unit = 
    let (command : SQLiteCommand) = new SQLiteCommand(statement, conn)
    ignore <| command.ExecuteNonQuery ()

let testConn (conn:SQLiteConnection) : unit =  
    let query = "SELECT * FROM sites;"
    let command = new SQLiteCommand(query, conn)
    let reader : SQLiteDataReader = command.ExecuteReader()
    while reader.Read() do
        printfn "%s %s" (reader.GetString(0)) (reader.GetString(2))

let deleteAllTableRows (conn:SQLiteConnection) (name:string) : int =  
    let query = sprintf "DELETE FROM %s;" name
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery ()

let deleteAllSites (conn:SQLiteConnection) : int =  deleteAllTableRows conn "sites"

let deleteAllLocations (conn:SQLiteConnection) : int =  deleteAllTableRows conn "locations"



let insertDummy (conn:SQLiteConnection) = 
    let query = "INSERT INTO sites (sai_number, postal_address1, post_code) VALUES ('SAI00482900', 'YORKSTONE CLOSE', 'S2 5DN');"
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery ()


type SiteRow = ExcelFile< @"G:\\work\\NEXT_GEN_SURVEYS\\Supplied Info\\AI Data.xlsx",
                            SheetName = "qInstallationAddressesAndRespOf",
                            ForceString = true>


let insertSiteRow (conn:SQLiteConnection) (site1:SiteRow.Row) : unit = 
    let stmt =  sprintf "INSERT INTO sites (sai_number, common_name, \
                         postal_address1, postal_address2, postal_address3, postal_address4, \
                         post_code) \
                         VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s');"
                         site1.InstReference 
                         site1.InstCommonName
                         (cleanseValue <| site1.``Postal Address 1``)
                         (cleanseValue <| site1.``Postal Address 2``)
                         (cleanseValue <| site1.``Postal Address 3``)
                         (cleanseValue <| site1.``Postal Address 4``)
                         (cleanseValue <| site1.``Post Code``)
    runNonQuery conn stmt

let insertLocationRow (conn:SQLiteConnection) (site1:SiteRow.Row) : unit = 
    let stmt = sprintf "INSERT INTO locations (common_name, \
                         postal_address1, postal_address2, postal_address3, postal_address4, \
                         post_code) \
                         VALUES ('%s', '%s', '%s', '%s', '%s', '%s');"
                         (cleanseValue <| site1.InstCommonName)
                         (cleanseValue <| site1.``Postal Address 1``)
                         (cleanseValue <| site1.``Postal Address 2``)
                         (cleanseValue <| site1.``Postal Address 3``)
                         (cleanseValue <| site1.``Postal Address 4``)
                         (cleanseValue <| site1.``Post Code``)
    runNonQuery conn stmt

let insertAllSites (conn:SQLiteConnection) = 
    let file = new SiteRow()
    let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
    for rowi in file.Data do
        insertSiteRow conn rowi
        printfn "%s %s" rowi.InstAssetId  rowi.InstCommonName
    trans.Commit ()

let insertAllLocations (conn:SQLiteConnection) = 
    let file = new SiteRow()
    let foldStep (s1 : Set<string>) (x:SiteRow.Row) = 
        let name = x.SiteCommonName
        if s1.Contains name then s1 
        else insertLocationRow conn x
             printfn "%s %s" x.InstAssetId  x.InstCommonName
             s1.Add name
    let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
    let _ = file.Data |> Seq.fold foldStep Set.empty
    trans.Commit () 

    


