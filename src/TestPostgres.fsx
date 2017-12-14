#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#r "Npgsql"
open Npgsql
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"

#load "PostgreSQLUtils.fs"
open PostgreSQLUtils

let connString1 : Printf.StringFormat<(string -> string)> = "Host=localhost;Username=postgres;Password=%s;Database=spt_test";

let test01 (pwd:string) = 
    let connString = sprintf connString1 pwd 
    let conn = new NpgsqlConnection(connString)
    conn.Open ()
    let cmd = new NpgsqlCommand("SELECT name, age FROM people", conn)
    let reader = cmd.ExecuteReader()
    while reader.Read() do
        printfn "%s %i" (reader.GetString(0)) (reader.GetInt64(1))
    conn.Close ()

