[<AutoOpen>]
module AssetRep.CsoOutputs

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


let runNonQuery (conn:SQLiteConnection) (statement:string) : unit = 
    let (command : SQLiteCommand) = new SQLiteCommand(statement, conn)
    ignore <| command.ExecuteNonQuery ()

type UltrasonicsRow = ExcelFile< @"G:\work\CSO_Outputs\ultrasonics.xlsx",
                                    SheetName = "Sheet1",
                                    ForceString = true>

let insertUltrasonicsRow (conn:SQLiteConnection) (row1:UltrasonicsRow.Row) : unit = 
    let stmt = sprintf "INSERT INTO ultrasonics (reference, \
                                           common_name, \
                                           site_name, \
                                           installed_from, \
	                                       manufacturer, \
                                           model, \
                                           asset_status ) \
                         VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s');"
                         row1.Reference
                         (escapeValueText <| row1.``Common Name``)
                         (escapeValueText <| "TODO")
                         (escapeValueText <| row1.``Installed From``)
                         (escapeValueText <| row1.Manufacturer)
                         (escapeValueText <| row1.Model)
                         (escapeValueText <| row1.AssetStatus)
    runNonQuery conn stmt

let insertAllUltrasonics (conn:SQLiteConnection) = 
    let file = new UltrasonicsRow()
    let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
    for rowi in file.Data do
        insertUltrasonicsRow conn rowi
        printfn "%s %s" rowi.Reference  rowi.``Common Name``
    trans.Commit ()

let deleteAllUltrasonics (conn:SQLiteConnection) : int =  
    let query = sprintf "DELETE FROM ultrasonics;"
    let (command : SQLiteCommand) = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery ()