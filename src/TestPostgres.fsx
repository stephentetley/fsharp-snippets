#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load @"ResultMonad.fs"
#load @"SqlUtils.fs"
#load @"PGSQLConn.fs"
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


let test03 (pwd:string) : unit = 
    let connstring = makeConnString pwd "spt_geo" 
    let proc = 
        execReader "SELECT point_code, point_name FROM temp_routing" <| fun reader -> 
            while reader.Read() do 
               printfn "%s, %s" (reader.GetString(0)) (reader.GetString(1)) 
    ignore <| runPGSQLConn proc connstring 

let test04 (pwd:string) = 
    let connstring = makeConnString pwd "spt_geo" 
    let query = @"SELECT ST_AsGeoJSON(ST_GeomFromText('MULTIPOINT(50 5, 150 30, 50 10, 10 10)')) ;"
    let proc = 
        execReaderSingleton query <| fun reader -> printfn "%s" (reader.GetString(0))
    ignore <| runPGSQLConn proc connstring 


let test05 (pwd:string) : unit = 
    let connstring = makeConnString pwd "spt_geo" 
    let proc = 
        execReaderList "SELECT point_code, point_name FROM temp_routing;" <| fun reader -> 
            printfn "%s, %s" (reader.GetString(0)) (reader.GetString(1)) 
    ignore <| runPGSQLConn proc connstring 
