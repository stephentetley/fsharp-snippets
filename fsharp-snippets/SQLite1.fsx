#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"

open System.Data.SQLite

// Need to set working directory so Data Source works...
open System
System.Environment.CurrentDirectory <- @"E:\coding\fsharp\fsharp-snippets\fsharp-snippets"

let dbconn = new SQLiteConnection( "Data Source=..\data\db1.sqlite;Version=3;")
dbconn.Open()

(*
let query = "insert into highscores (name, score) values ('Me', 3000)"
let command = new SQLiteCommand(query, dbconn)
command.ExecuteNonQuery()

query = "insert into highscores (name, score) values ('Myself', 6000)"
command = new SQLiteCommand(query, dbconn)
command.ExecuteNonQuery()
 
query = "insert into highscores (name, score) values ('And I', 9001)"
command = new SQLiteCommand(query, dbconn)
command.ExecuteNonQuery()
*)


let query4 : string = "select * from highscores"
let command4 = new SQLiteCommand(query4, dbconn)

let reader : SQLiteDataReader = command4.ExecuteReader()
while reader.Read() do
    printf "%s %d\n" (reader.GetString(0)) (reader.GetInt32(1))

reader.Close()

dbconn.Close()
