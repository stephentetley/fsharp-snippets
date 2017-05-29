// Hazards should probably go in a sqlite database.

// datetime - http://stackoverflow.com/questions/1933720/how-do-i-insert-datetime-value-into-a-sqlite-database

// nuget install Sqlite.Core and EntityFramework before SQLite 

#I @"..\packages\System.Data.SQLite.Core.1.0.105.1\lib\net451"
#r "System.Data.SQLite"

#I @"..\packages\ExcelProvider.0.8.1\lib"
#r "ExcelProvider.dll"

#load "Utils.fs"

open FSharp.ExcelProvider
open Microsoft.FSharp.Math
open System.Data.SQLite
open System
open System.Globalization

open Hazards.Utils

// Need to set working directory so Data Source works...

System.Environment.CurrentDirectory <- @"E:\coding\fsharp\hazards\hazards"


let connstring = "Data Source=..\data\hazards.sqlite;Version=3;"


type HazardRow = ExcelFile< @"..\data\HaZaRdS short 3 5 17.xlsx",
                            SheetName = "Sheet1",
                            ForceString = false >



let runCommand (conn:SQLiteConnection) (cmd:string) : int =
    let command = new SQLiteCommand(cmd, conn)
    command.ExecuteNonQuery()

let conn = new SQLiteConnection(connstring)
conn.Open()


// Apparently no 'TRUNCATE TABLE' in Sqlite
let cleardb () = 
    ignore <| runCommand conn "DELETE FROM hazards;"
    
let cleardb1 = 
    let query = "DELETE FROM hazards;"
    let command = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery()

// conn.Close()

let viewdb () = 
    let query = "select * from hazards"
    let command = new SQLiteCommand(query, conn)
    let reader : SQLiteDataReader = command.ExecuteReader()
    while reader.Read() do
        printf "%i, %s\n" (reader.GetInt64(0))  (reader.GetString(2))


let testfill () = 
    let query = "insert into hazards (ID, status) values (1000, 'Submitted')"
    let command = new SQLiteCommand(query, conn)
    command.ExecuteNonQuery()

// filldb


let file = new HazardRow()

// let castID (x : float) = 
//    if x = null then -1 else int x

for rowi in file.Data do
   printfn "%d %s '%s' '%s'" (int64 rowi.``ID``) 
                         rowi.``Accident category`` 
                         (rowi.``Date occured`` |> decodeTime |> encodeTime) 
                         rowi.``Description``


let clearCmd : string = 
    "TRUNCATE TABLE hazards"
    



let makeIns1 (key:int64) : string option = 
    if key > 0L then sprintf "insert into hazards (ID) values (%i);" key |> Some else None
                

let atime = System.DateTime.ParseExact("03/05/2017 08:40:00", "dd/MM/yyyy hh:mm:ss", Globalization.CultureInfo.InvariantCulture ) in atime

let now = System.DateTime.Now in printf "%s\n" (now.ToString("yyyy-MM-dd hh:mm:ss"))

for rowi in file.Data do
   printfn "%u" (rowi.``ID`` |> int64)
