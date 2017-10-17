#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"

open System.Data.SQLite


let constring = 
    let s1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\db1.sqlite")
    sprintf "Data Source=%s;Version=3;" s1


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

let test01 () = 
    let dbconn = new SQLiteConnection(constring)
    dbconn.Open()
    let query1 : string = "select * from highscores"
    let command1 = new SQLiteCommand(query1, dbconn)
    let reader : SQLiteDataReader = command1.ExecuteReader()
    while reader.Read() do
        printf "%s %d\n" (reader.GetString(0)) (reader.GetInt32(1))
    reader.Close()
    dbconn.Close()
