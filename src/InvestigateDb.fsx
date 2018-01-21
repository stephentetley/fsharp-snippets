#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite


#I @"..\\packages\SQLProvider.1.0.54\lib"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql


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

let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\edmDB.sqlite3;Version=3"

type SqlDB = SqlDataProvider< 
              ConnectionString = ConnectionString1,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = ResolutionPath1,
              IndividualsAmount = 1000,
              UseOptionTypes = true >
let ctx = SqlDB.GetDataContext()

let distinctAssetsTP () : string list = 
    query { for a in ctx.Main.CatsConsents do
            select (a.AssetName) }
        |> Seq.toList

type DistinctAsset = 
    { SaiNumber: string
      AssetName: string
      WorkCategory: string }

// outlet_ngr has poor data
let genDistinctAssetsQuery () : string = 
    System.String.Format("""
        SELECT DISTINCT 
            asset_sai_number, 
            asset_name, 
            work_category,
            outlet_ngr
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

let test01 () = 
    distinctAssetsTP () |> List.iter (printfn "%s")