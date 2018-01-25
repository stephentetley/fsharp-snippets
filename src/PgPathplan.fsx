#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql


#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\Coord.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.Geo.Coord
open SL.PGSQLConn
open SL.ScriptMonad
open SL.CsvOutput

// PostgresSQL with PostGIS enabled.
// Table Schema:
// CREATE TABLE spt_outfalls (stc25_ref VARCHAR(12) PRIMARY KEY, function_node VARCHAR(30), osgb36_grid VARCHAR(16), point_loc geography (POINT));



type PathImportTable = 
    CsvProvider< @"G:\work\Projects\events2\pathplan-mock-data.csv",
                 HasHeaders = true>

type PathImportRow = PathImportTable.Row

let getPathImportRows () : PathImportRow seq = (new PathImportTable ()).Rows |> Seq.cast<PathImportRow>


// ********** SCRIPT **********
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)

let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRows "spt_pathfind"

let makePathSectionINSERT (row:PathImportRow) : string = 

    let makePointLit (pt:WGS84Point) : string = 
        // SRID=4326 is WGS 84 coordinate reference system
        sprintf "ST_GeogFromText('SRID=4326;POINT(%f %f)')"
                pt.Longitude pt.Latitude

    // Note the id column is PG's SERIAL type so it is inserted automatically
    sqlINSERT "spt_pathfind" 
        <|  [ stringValue       "basetype"          row.BASETYPE
            ; stringValue       "function_node"     row.FUNCTION_Link
            ; literalValue      "start_point"       "NULL"
            ; literalValue      "end_point"         "NULL"
            ]



let insertOutfalls () : Script<int> = 
    let rows = getPathImportRows ()
    let proc1 (row:PathImportRow) : PGSQLConn<int> = 
        execNonQuery <| makePathSectionINSERT row
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 rows



let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
  
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ deleteAllData ()          |> logScript (sprintf "%i rows deleted")
            ; insertOutfalls ()         |> logScript (sprintf "%i rows inserted") 
            ]
