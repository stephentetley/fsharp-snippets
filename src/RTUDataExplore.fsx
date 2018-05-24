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
#load @"SL\CsvUtils.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.CsvUtils

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

// Need to edit the input to remove (@) 
let makeSqlPointDataRow (csvRow:PointReportRow) : unit = 
    let trimOption = Option.map (fun (s:string) -> s.Trim())
    let row = sqlPointData.Create()
    let os,point = splitOsPoint csvRow.``OS\Point name``
    row.OsName <- os
    row.PointName <- point
    row.PointComment <- trimOption << nullToOption <| csvRow.``Point comment``.Trim()
    row.TypeAndIx <- trimOption << nullToOption <| csvRow.``Der. Typ. No``
    row.OdName <- trimOption << nullToOption <| csvRow.``OD name``
    row.PointFunction <- trimOption << nullToOption <| csvRow.Function
    row.RecordingInfo <- trimOption << nullToOption <| csvRow.``Recording & timebase (on DG: on archive)``
    row.ScalingOrMnemonics <- trimOption << nullToOption <| csvRow.``Scaling (raw=scaled)/mnemonics``
    row.AlarmParameters <- trimOption << nullToOption <| csvRow.``Alarm parameters``
    row.ControlPicAlarmPic <- trimOption << nullToOption <| csvRow.``Ctrl pic  Alarm pic``



let dbAddOsRecords () : unit = 
    (new OsReport()).Rows 
        |> Seq.toList
        |> List.map makeSqlOsDataRow
        |> ignore
    sqlCtx.SubmitUpdates ()


let dbAddPointRecords () : unit = 
    (new PointReport()).Rows 
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
    dbAddPointRecords () 

type PointDerived = Real | Derived | Calculated

/// S A I
type PointType = Analogue | Status | Integrated

let pointDerived : Parser<PointDerived,unit> = 
    (pchar 'D' |>> fun _ -> Derived)        <|> 
    (pchar 'R' |>> fun _ -> Real)           <|>
    (pchar 'C' |>> fun _ -> Calculated)

let pointType : Parser<PointType,unit> = 
    (pchar 'S' |>> fun _ -> Status)   <|> 
    (pchar 'A' |>> fun _ -> Analogue)   <|>
    (pchar 'I' |>> fun _ -> Integrated)

let parseDTI : Parser<PointDerived * PointType * int, unit> = 
    tuple3 (pointDerived .>> spaces) (pointType .>> spaces) pint32

let (|||) (f:char -> bool) (g:char -> bool) : char -> bool = 
    fun ch -> f ch || g ch

let mnemonic1 : Parser<string,unit> = 
    many1Satisfy2 (System.Char.IsUpper) 
                    (System.Char.IsLetterOrDigit ||| (fun c -> c='_')) 

let mnemonics : Parser<string list, unit> = 
    sepBy1 mnemonic1 spaces1

let pdecimal = pfloat |>> decimal

type Scaling = int * decimal

let scaling1 : Parser<Scaling,unit> = 
    tuple2 (pint32 .>> pchar '=') pdecimal

let scalings : Parser<Scaling list,unit> = 
    sepBy1 scaling1 (pchar ',' .>> spaces1)

type AlarmLocation = OS | DG

let alarmLocation : Parser<AlarmLocation, unit> = 
    (pstring "OS" |>> fun _ -> OS)  <|>
    (pstring "DG" |>> fun _ -> DG) 

// TO COMPLETE!
let alarmParameters : Parser<AlarmLocation * string, unit> = 
    tuple2 (alarmLocation .>> spaces1) (restOfLine false)

let readDTI (input:string) : option<PointDerived * PointType * int> = 
    match run parseDTI input with
    | Success(a,_,_) -> Some a
    | Failure(s,_,_) -> failwithf "dti - %s" input //  printfn "%s" s; None

let runrun (p:Parser<'a,unit>) (input:string) : option<'a> = 
    match run p input with
    | Success(a,_,_) -> Some a
    | Failure(s,_,_) -> failwithf "runrun - %s" input //  printfn "%s" s; None

let temp01 () = readDTI "C    A    1"

let temp02 () : unit = 
    query { 
        for c in sqlCtx.Main.PointData do 
        select (c.TypeAndIx) 
        } |> Seq.iter (Option.iter (printfn "%A" << readDTI))


let temp03 () = 
    let input  = @"G:\work\Projects\events2\data\rtu-data\rtu-tad-all-points.tab.csv"
    let output = @"G:\work\Projects\events2\data\rtu-data\rtu-tad-all-points.trim.csv"
    let options = 
        { InputSeparator = "\t"
          InputHasHeaders = true
          OutputSeparator = "," }
    trimCsvFile options input output

let temp04 () = runrun mnemonics "ISOLATED POWER_ON"
let temp05 () = runrun scalings "2000=-20.0000, 16000=85.0000"

let temp06 () = runrun alarmParameters "DG OFF/E ON/E TDB=60s"