#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
open SL.PGSQLConn


let makeConnParams (pwd:string) (dbname:string) : PGSQLConnParams = 
    pgsqlConnParamsTesting dbname pwd


let test01 (pwd:string) = 
    let connString = paramsConnString <| makeConnParams pwd "spt_test" 
    let conn = new NpgsqlConnection(connString)
    conn.Open ()
    let cmd = new NpgsqlCommand("SELECT name, age FROM people", conn)
    let reader = cmd.ExecuteReader()
    while reader.Read() do
        printfn "%s %i" (reader.GetString(0)) (reader.GetInt64(1))
    conn.Close ()

let test02 (pwd:string) : unit = 
    let connparams = makeConnParams pwd "spt_test" 
    let proc = 
        execReader "SELECT name, age FROM people" <| fun reader -> 
            while reader.Read() do 
               printfn "%s is %i years old" (reader.GetString(0)) (reader.GetInt64(1)) 
    ignore <| runPGSQLConn proc connparams 


let test03 (pwd:string) : unit = 
    let connparams = makeConnParams pwd "spt_geo" 
    let proc = 
        execReader "SELECT point_code, point_name FROM temp_routing" <| fun reader -> 
            while reader.Read() do 
               printfn "%s, %s" (reader.GetString(0)) (reader.GetString(1)) 
    ignore <| runPGSQLConn proc connparams 

let test04 (pwd:string) = 
    let connparams = makeConnParams pwd "spt_geo" 
    let query = @"SELECT ST_AsGeoJSON(ST_GeomFromText('MULTIPOINT(50 5, 150 30, 50 10, 10 10)')) ;"
    let proc = 
        execReaderSingleton query <| fun reader -> printfn "%s" (reader.GetString(0))
    ignore <| runPGSQLConn proc connparams 


let test05 (pwd:string) : unit = 
    let connparams = makeConnParams pwd "spt_geo" 
    let proc = 
        execReaderList "SELECT point_code, point_name FROM temp_routing;" <| fun reader -> 
            printfn "%s, %s" (reader.GetString(0)) (reader.GetString(1)) 
    ignore <| runPGSQLConn proc connparams 
