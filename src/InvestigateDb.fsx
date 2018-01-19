#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#load @"SL\Coord.fs"
#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\ScriptMonad.fs"
open SL.Geo.Coord
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad



type DistinctAsset = 
    { SaiNumber: string
      AssetName: string
      WorkCategory: string }

// Note TargetPercent of 1.0 gives a convex hull (0.9 seems okay)
let genDistinctAssetsQuery () : string = 
    System.String.Format("""
        SELECT DISTINCT 
            asset_sai_number, 
            asset_name, 
            work_category 
        FROM 
            cats_consents
    """)

// Force the seq to a List otherwise the connection appears to close with returning
// any values.
let distinctAssetsQuery : SQLiteConn<DistinctAsset list> = 
    let query = genDistinctAssetsQuery ()
    let procM (reader:SQLiteDataReader) : DistinctAsset = 
        { SaiNumber     = reader.GetString(0)
        ; AssetName     = reader.GetString(1)
        ; WorkCategory  = reader.GetString(2) }
    execReaderList query procM   

type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:SQLiteConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)

let distinctAssets : Script<DistinctAsset list> = 
    liftWithConnParams <| runSQLiteConn distinctAssetsQuery

let main () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\edmDB.sqlite3"
  
    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn 
        <| fmapM (List.map (fun o -> o.AssetName)) distinctAssets