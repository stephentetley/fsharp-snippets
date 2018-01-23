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
open SL.AnswerMonad
open SL.SqlUtils
open SL.Geo.Coord
open SL.PGSQLConn
open SL.ScriptMonad


type GisOutfallData = 
    CsvProvider< @"G:\work\Projects\events2\db-import-tables\gis-outlets-wkt.csv",
                 HasHeaders = true>

type GisOutfallRow = GisOutfallData.Row

let getGisOutfalls () : GisOutfallRow list = (new GisOutfallData ()).Rows |> Seq.toList

// ********** SCRIPT **********
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)

let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRows "spt_outfalls"

let makeOutfallINSERT (row:GisOutfallRow) : string = 
    let east    = 1.0<meter> * (float <| row.METREEASTING)
    let north   = 1.0<meter> * (float <| row.METRENORTHING)
    let wgs84Pt = osgb36PointToWGS84 <| { Easting = east; Northing = north }
    let pointLit = 
        sprintf "ST_GeogFromText('SRID=4326;POINT(%f %f)')"
                wgs84Pt.Longitude wgs84Pt.Latitude

    sqlINSERT "spt_outfalls" 
        <|  [ stringValue       "stc25_ref"         row.STC25_REF
            ; stringValue       "function_node"     row.FUNCTION_NODE
            ; literalValue      "point_loc"         pointLit
            ]

let insertOutfalls () : Script<int> = 
    let rows = getGisOutfalls ()
    let proc1 (row:GisOutfallRow) : PGSQLConn<int> = execNonQuery <| makeOutfallINSERT row
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumForM rows proc1

let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
  
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ deleteAllData ()          |> logScript (sprintf "%i rows deleted")
            ; insertOutfalls ()         |> logScript (sprintf "%i rows inserted") 
            ]
            
