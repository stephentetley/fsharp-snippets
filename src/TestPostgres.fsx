#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql


#load @"SqlUtils.fs"
#load "PGSQLConn.fs"
open PGSQLConn


let makeConnString (pwd:string) (dbname:string) : string = 
    let fmt : Printf.StringFormat<(string -> string -> string)> = "Host=localhost;Username=postgres;Password=%s;Database=%s";
    sprintf fmt pwd dbname


let test01 (pwd:string) = 
    let connString = makeConnString pwd "spt_test" 
    let conn = new NpgsqlConnection(connString)
    conn.Open ()
    let cmd = new NpgsqlCommand("SELECT name, age FROM people", conn)
    let reader = cmd.ExecuteReader()
    while reader.Read() do
        printfn "%s %i" (reader.GetString(0)) (reader.GetInt64(1))
    conn.Close ()

let test02 (pwd:string) : unit = 
    let connstring = makeConnString pwd "spt_test" 
    let proc = 
        execReader "SELECT name, age FROM people" <| fun reader -> 
            while reader.Read() do 
               printfn "%s is %i years old" (reader.GetString(0)) (reader.GetInt64(1)) 
    ignore <| runPGSQLConn proc connstring 