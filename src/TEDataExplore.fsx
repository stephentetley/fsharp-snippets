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
    { RoofSlabToInvert: float<meter> 
      TransducerFaceToInvert: float<meter> 
      ScreenBottomToInvert: float<meter>
      OverflowToInvert: float<meter>
      EmergencyOverflowToInvert: float<meter> }

type CsoRecord = 
    { SiteName: string
      CsoIndex: string
      Measurements: CsoMeasurements }


let readWithZeroDefault (s:string) : float<meter> = 
    try
        System.Convert.ToDouble(s) * 1.0<meter>
    with
    | _ -> 0.0<meter>

let readCsoH (row:HawkeyeCsoRow) : CsoRecord = 
    let measurements = 
        { RoofSlabToInvert = readWithZeroDefault row.``ROOF SLAB TO INVERT``
          TransducerFaceToInvert = readWithZeroDefault row.``UNDERSIDE OF U/S FACE TO INVERT ``
          ScreenBottomToInvert = 0.0<meter>
          OverflowToInvert = readWithZeroDefault row.``OVERFLOW LEVEL TO INVERT``
          EmergencyOverflowToInvert = readWithZeroDefault row.``EMERGENCY OVERFLOW LEVEL TO INVERT``
          }
    { SiteName = row.Site
      CsoIndex = "CSO(1)"
      Measurements = measurements }

let readBest (oldVal:string) (newVal:string): float<meter> = 
    try
        System.Convert.ToDouble(newVal) * 1.0<meter>
    with
    | _ -> 
        try 
            System.Convert.ToDouble(oldVal) * 1.0<meter>
        with 
        | _ -> 0.0<meter>


let readCsoPs (row:PoweredSiteCsoRow) : CsoRecord = 
    let measurements = 
        { RoofSlabToInvert = 
                readBest row.``Old ROOF SLAB TO INVERT`` row.``New ROOF SLAB TO INVERT``
      
          TransducerFaceToInvert = 
                readBest row.``New UNDERSIDE OF U/S TO INVERT`` row.``Old UNDERSIDE OF U/S TO INVERT``
      
          ScreenBottomToInvert = 
                readBest row.``Old BOTTOM OF SCREEN TO INVERT`` row.``New BOTTOM OF SCREEN TO INVERT``
      
          OverflowToInvert = 
                readBest row.``Old OVERFLOW LEVEL TO INVERT`` row.``New OVERFLOW LEVEL TO INVERT``
      
          EmergencyOverflowToInvert = 
            readBest row.``Old EMERGENCY OVERFLOW LEVEL TO INVERT`` row.``New EMERGENCY OVERFLOW LEVEL TO INVERT``
          }
    { SiteName = row.Site
      CsoIndex = row.CSO
      Measurements = measurements }
      
      
let optionMeasure (d:float<meter>) : decimal option = 
    if d <= 0.0<meter> then 
        None 
    else
        Some (decimal d)



let makeDbRow (cso:CsoRecord) : unit = 
    let row = sqlCsoChambers.Create()
    row.SiteName <- cso.SiteName
    row.CsoIndex <- cso.CsoIndex
    row.RoofSlabToI <- optionMeasure cso.Measurements.RoofSlabToInvert



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
let dbAddRecords () : unit = 
    getHawkeyeCsoRows () 
        |> Seq.toList
        |> List.map (makeDbRow << readCsoH) 
        |> ignore
    sqlCtx.SubmitUpdates ()

let testAdd1 (name:string) : unit = 
    let row1 = sqlCsoChambers.``Create(cso_index, site_name)`` ("CSO(1)", name)
    sqlCtx.SubmitUpdates()

let testAdd2 (name:string) : unit = 
    let row1 = sqlCsoChambers.Create()
    row1.SiteName <- name
    row1.CsoIndex <- "CSO(1)"
    sqlCtx.SubmitUpdates()