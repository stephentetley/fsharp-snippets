#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net46"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\\packages\SQLProvider.1.1.41\lib\net451"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\FParsec.1.0.3\lib\net40-client"
#r "FParsec"
#r "FParsecCS"
open FParsec



open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"Scripts\RtuData.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open Scripts.RtuData

type OsReport = 
    CsvProvider< 
        Sample = @"G:\work\Projects\events2\data\rtu-data\os-report.trim.csv",
        HasHeaders = true,
        IgnoreErrors = true >

type OsReportRow = OsReport.Row


type PointReport = 
    CsvProvider< 
        Sample = @"G:\work\Projects\events2\data\rtu-data\rtu-tad-all-points.trim.csv",
        HasHeaders = true,
        IgnoreErrors = true >

type PointReportRow = PointReport.Row


let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\data\rtu-data\rtuDB.sqlite3;Version=3"


type SqlDB = 
    SqlDataProvider< 
        ConnectionString = ConnectionString1,
        DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
        ResolutionPath = ResolutionPath1,
        IndividualsAmount = 1000,
        UseOptionTypes = true >

let sqlCtx : SqlDB.dataContext = SqlDB.GetDataContext()


type SqlOsDataTable = SqlDB.dataContext.mainSchema.``main.os_data``
type SqlOsDataRow = SqlDB.dataContext.``main.os_dataEntity``

let sqlOsData : SqlOsDataTable = sqlCtx.Main.OsData

type SqlPointDataTable = SqlDB.dataContext.mainSchema.``main.point_data``
type SqlPointDataRow = SqlDB.dataContext.``main.point_dataEntity``

let sqlPointData : SqlPointDataTable = sqlCtx.Main.PointData



let nullToOption (s:string) : Option<string> = 
    match s with
    | null -> None
    | _ -> Some s

let makeSqlOsDataRow (csvRow:OsReportRow) : unit = 
    let row = sqlOsData.Create()
    row.OsName <- csvRow.``OS name``
    row.SiteCode <- Some <| csvRow.``OD name``
    row.SiteName <- Some <| csvRow.``OD comment``
    row.SetName <- Some <| csvRow.``Set name``
    row.Media <- Some <| csvRow.Media
    row.OsComment <- Some <| csvRow.``OS comment``
    row.GridRef <- nullToOption <| csvRow.``Grid ref``
    row.ScanSchedule <- nullToOption <| csvRow.``Scan sched``
    row.IpAddress <- nullToOption <| csvRow.``IP Address,  1'ary IP Route,2'ary IP Route``
    row.OsAddress <- nullToOption <| csvRow.``OS Addr``
    row.OsType <- nullToOption <| csvRow.``OS type``
    row.ParentOu <- nullToOption <| csvRow.``Parent OU``
    row.ParentOuComment <- nullToOption <| csvRow.``Parent OU Comment``

let splitOsPoint (s:string) : string * string = 
    let arr : string [] = s.Split(separator=[| '\\' |], count=2)
    if arr.Length = 2 then
        (arr.[0].Trim() , arr.[1].Trim ())
    else 
        "", ""


let readDTI (input:string) : option<QualifiedType * int> = 
    match run parseDerTypNo input with
    | Success(a,_,_) -> Some a
    | Failure(s,_,_) -> failwithf "dti - %s" input //  printfn "%s" s; None


// Need to edit the input to remove (@) 
let makeSqlPointDataRow (csvRow:PointReportRow) : unit = 
    let trimOption = Option.map (fun (s:string) -> s.Trim())
    let row = sqlPointData.Create()
    let os,point = splitOsPoint csvRow.``OS\Point name``

    row.OsName <- os
    row.PointName <- point
    row.PointComment <- trimOption << nullToOption <| csvRow.``Point comment``.Trim()
    match csvRow.``Der. Typ. No`` |> nullToOption |> Option.bind readDTI with
    | Some(qt,ix) ->
        row.PointIndex <- Some <| int64 ix
        row.PointType <- Some <| longhandQualifiedType qt
    | None -> () 

    row.OdName              <- trimOption << nullToOption <| csvRow.``OD name``
    row.PointFunction       <- trimOption << nullToOption <| csvRow.Function
    row.RecordingInfo       <- trimOption << nullToOption <| csvRow.``Recording & timebase (on DG: on archive)``
    row.ScalingOrMnemonics  <- trimOption << nullToOption <| csvRow.``Scaling (raw=scaled)/mnemonics``
    row.AlarmParameters     <- trimOption << nullToOption <| csvRow.``Alarm parameters``
    row.ControlPicAlarmPic  <- trimOption << nullToOption <| csvRow.``Ctrl pic  Alarm pic``



let dbAddOsRecords () : unit = 
    (new OsReport()).Rows 
        |> Seq.toList
        |> List.map makeSqlOsDataRow
        |> ignore
    sqlCtx.SubmitUpdates ()


let dbAddPointRecords (csvFilePath:String) : unit =
    let csvData = PointReport.Load(csvFilePath)
    csvData.Rows
        |> Seq.toList
        |> List.map makeSqlPointDataRow
        |> ignore
    sqlCtx.SubmitUpdates ()
    

let dbDeleteOsRecords () : int = 
    query { 
        for c in sqlCtx.Main.OsData do 
        where (c.OsName <> "")
        } 
        |> Seq.``delete all items from single table``
        |> Async.RunSynchronously

let dbDeletePointRecords () : int = 
    query { 
        for c in sqlCtx.Main.PointData do 
        where (c.OsName <> "")
        } 
        |> Seq.``delete all items from single table``
        |> Async.RunSynchronously



let setupDB () : unit = 
    dbDeleteOsRecords () |> ignore
    dbDeletePointRecords () |> ignore
    dbAddOsRecords ()
    dbAddPointRecords  @"G:\work\Projects\events2\data\rtu-data\rtu-S_WW_LEE-points.trim.csv"
    dbAddPointRecords  @"G:\work\Projects\events2\data\rtu-data\rtu-tad-all-points.trim.csv"


let runrun (p:Parser<'a,unit>) (input:string) : option<'a> = 
    match run p input with
    | Success(a,_,_) -> Some a
    | Failure(s,_,_) -> failwithf "runrun - %s" input //  printfn "%s" s; None

let temp01 () = readDTI "C    A    1"

let temp02 () : unit = 
    query { 
        for c in sqlCtx.Main.PointData do 
        select (c.PointType, c.PointIndex) 
        } |> Seq.iter (printfn "%A")




let temp04 () = runrun parseMnemonics "ISOLATED POWER_ON"
let temp05 () = runrun parseScalings "2000=-20.0000, 16000=85.0000"

// "OS HH=1.0/A/2/I HI=Discard LO=Discard LL=Discard TDB=60s"

let temp06 ()  = runrun parseAlarmParameters @"DG OFF/E ON/E TDB=60s"
let temp06a () = runrun parseAlarmParameters @"OS HH=1.0/A/2/I HI=Discard LO=Discard LL=Discard TDB=60s"
let temp06b () = runrun parseAlarmParameters @"DG TDB=60s"
let temp06c () = runrun parseAlarmParameters @"OS HH=3.50000/A/2/I HI=3.30000/A/2/I LO=1.70000/E/3/N LL=1.50000/E/3/N Dead=0.05000 TDB=300s"


// "Val@15M(45d:A),1H(45d:A)"
// "Log@24H(8d:A)  Val@15M(8d:A)"
let temp07 ()  = runrun parserRecordingInfos @"Log@24H(8d:A)  Val@15M(8d:A)"
let temp07a () = runrun parserRecordingInfos @"Val@15M(45d:A),1H(45d:A)"
let temp07b () = runrun parserRecordingInfos @"Val@15M(8d:A) Ave@15M(8d:A),24H(8d:A)"


let temp08 () = 
    printfn "%s" <| ("hello".PadRight 32) + "world!"
    printfn "%s" <| ("a string that is a bit".PadRight 32) + "longer"
