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


open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\ExcelProviderHelper.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.ExcelProviderHelper


type HawkeyeCsoTable = 
    ExcelFile< @"G:\work\Projects\events2\telemetry-enhancements\hawkeyes-edit.xlsx",
                SheetName = "CSO",
                ForceString = true >

type HawkeyeCsoRow = HawkeyeCsoTable.Row

let getHawkeyeCsoRows () : seq<HawkeyeCsoRow> = 
    let dict : GetRowsDict<HawkeyeCsoTable, HawkeyeCsoRow> = 
        { GetRows     = fun table -> table.Data 
          NotNullProc = 
            fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRowsSeq dict (new HawkeyeCsoTable())


type PoweredSiteCsoTable = 
    ExcelFile< @"G:\work\Projects\events2\telemetry-enhancements\powered-sites-edit.xlsx",
                SheetName = "CSO",
                ForceString = true >

type PoweredSiteCsoRow = PoweredSiteCsoTable.Row



let getPoweredSiteCsoRows () : seq<PoweredSiteCsoRow> = 
    let dict : GetRowsDict<PoweredSiteCsoTable, PoweredSiteCsoRow> = 
        { GetRows     = fun table -> table.Data 
          NotNullProc = 
            fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRowsSeq dict (new PoweredSiteCsoTable())


let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\telemetry-enhancements\te_measurements.sqlite3;Version=3"


type SqlDB = 
    SqlDataProvider< 
        ConnectionString = ConnectionString1,
        DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
        ResolutionPath = ResolutionPath1,
        IndividualsAmount = 1000,
        UseOptionTypes = true >

let sqlCtx : SqlDB.dataContext = SqlDB.GetDataContext()


let sqlCsoChambers : SqlDB.dataContext.mainSchema.``main.cso_chambers`` = sqlCtx.Main.CsoChambers

type SqlCsoRow = SqlDB.dataContext.``main.cso_chambersEntity``



// *************************************

type CsoMeasurements  = 
    { RoofSlabToInvert: decimal option
      TransducerFaceToInvert: decimal option
      ScreenBottomToInvert: decimal option
      OverflowToInvert: decimal option
      EmergencyOverflowToInvert: decimal option }

type CsoRecord = 
    { SiteName: string
      CsoIndex: string
      Measurements: CsoMeasurements }


let readOptDecimal (s:string) : decimal option = 
    try
        Some <| System.Convert.ToDecimal(s)
    with
    | _ -> None

let readCsoH (row:HawkeyeCsoRow) : CsoRecord = 
    let measurements = 
        { RoofSlabToInvert = readOptDecimal row.``ROOF SLAB TO INVERT``
          TransducerFaceToInvert = readOptDecimal row.``UNDERSIDE OF U/S FACE TO INVERT ``
          ScreenBottomToInvert = None
          OverflowToInvert = readOptDecimal row.``OVERFLOW LEVEL TO INVERT``
          EmergencyOverflowToInvert = readOptDecimal row.``EMERGENCY OVERFLOW LEVEL TO INVERT``
          }
    { SiteName = row.Site
      CsoIndex = "CSO(1)"
      Measurements = measurements }

let readBestDecimal (oldVal:string) (newVal:string): decimal option = 
    try
        Some <| System.Convert.ToDecimal(newVal)
    with
    | _ -> 
        try 
            Some <| System.Convert.ToDecimal(oldVal) 
        with 
        | _ -> None


let readCsoPs (row:PoweredSiteCsoRow) : CsoRecord = 
    let measurements = 
        { RoofSlabToInvert = 
                readBestDecimal row.``Old ROOF SLAB TO INVERT`` row.``New ROOF SLAB TO INVERT``
      
          TransducerFaceToInvert = 
                readBestDecimal row.``New UNDERSIDE OF U/S TO INVERT`` row.``Old UNDERSIDE OF U/S TO INVERT``
      
          ScreenBottomToInvert = 
                readBestDecimal row.``Old BOTTOM OF SCREEN TO INVERT`` row.``New BOTTOM OF SCREEN TO INVERT``
      
          OverflowToInvert = 
                readBestDecimal row.``Old OVERFLOW LEVEL TO INVERT`` row.``New OVERFLOW LEVEL TO INVERT``
      
          EmergencyOverflowToInvert = 
                readBestDecimal row.``Old EMERGENCY OVERFLOW LEVEL TO INVERT`` row.``New EMERGENCY OVERFLOW LEVEL TO INVERT``
          }
    { SiteName = row.Site
      CsoIndex = row.CSO
      Measurements = measurements }
      
      



let makeDbRow (cso:CsoRecord) : unit = 
    let row = sqlCsoChambers.Create()
    row.SiteName <- cso.SiteName
    row.CsoIndex <- cso.CsoIndex
    row.RoofSlabToI <- cso.Measurements.RoofSlabToInvert
    row.TransducerFaceToI <- cso.Measurements.TransducerFaceToInvert
    row.ScreenBottomToI <- cso.Measurements.ScreenBottomToInvert
    row.OverflowToI <- cso.Measurements.OverflowToInvert
    row.EmergencyOverflowToI <- cso.Measurements.EmergencyOverflowToInvert



let test01 () = 
    getHawkeyeCsoRows () 
        |> Seq.map readCsoH
        |> Seq.iter (printfn "%A") 


let test02 () = 
    getPoweredSiteCsoRows () 
        |> Seq.map (makeDbRow << readCsoPs)
        |> Seq.iter (printfn "%A") 


// Note - map a List not a Seq as we want to force a full traversal.
// Also, possibly a single fail, silently fails the whole transaction.
let dbAddCsoRecords () : unit = 
    getHawkeyeCsoRows () 
        |> Seq.toList
        |> List.map (makeDbRow << readCsoH) 
        |> ignore
    sqlCtx.SubmitUpdates ()

    getPoweredSiteCsoRows () 
        |> Seq.toList
        |> List.map (makeDbRow << readCsoPs) 
        |> ignore
    sqlCtx.SubmitUpdates ()


let dbDeleteCsoRecords () : int = 
    query { 
        for c in sqlCtx.Main.CsoChambers do 
        where (c.SiteName <> "")
        } 
        |> Seq.``delete all items from single table``
        |> Async.RunSynchronously
    

let testAdd1 (name:string) : unit = 
    let row1 = sqlCsoChambers.``Create(cso_index, site_name)`` ("CSO(1)", name)
    sqlCtx.SubmitUpdates()

let testAdd2 (name:string) : unit = 
    let row1 = sqlCsoChambers.Create()
    row1.SiteName <- name
    row1.CsoIndex <- "CSO(1)"
    sqlCtx.SubmitUpdates()