#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net45"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
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
#load @"SL\Coord.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.Geo.Coord

#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper


let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\discharges\erDischargeDB.sqlite3;Version=3"

type SqlDB = SqlDataProvider< 
              ConnectionString = ConnectionString1,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = ResolutionPath1,
              IndividualsAmount = 1000,
              UseOptionTypes = true >

let sqlCtx :SqlDB.dataContext = SqlDB.GetDataContext()



// ********** SCRIPT **********
type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftSQLiteConn (sql:SQLiteConn<'a>) : Script<'a> = 
    withConnParams <| fun conn -> liftAnswer <| runSQLiteConn conn sql


// ********** DATA SETUP **********

type DisXlsTable = 
    ExcelFile< @"G:\work\Projects\events2\discharges\discharge_list.xlsx",
                SheetName = "Installations",
                ForceString = true >

type DisXlsRow = DisXlsTable.Row

let getDischargesXls () : DisXlsRow list = 
    let dict : GetRowsDict<DisXlsTable, DisXlsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new DisXlsTable())



//  **** DB Import

let deleteAllData () : Script<int> = 
    let tables = 
        [ "discharges"
        ]
    liftSQLiteConn  << SL.SQLiteConn.sumSequenceM <| List.map deleteAllRows tables

// Note - can do CRUD with SQLProvider, but we may need to update dependencies?

//type DisDbTable = SqlDB.dataContext.mainSchema.``main.discharges``
//type DisDbRow = SqlDB.dataContext.``main.dischargesEntity``


// let d1 () = sqlCtx.Main.Discharges.Create()


let makeDisINSERT (row:DisXlsRow) : string = 
    sqlINSERT "discharges" 
        <|  [ stringValue       "site_code"         row.``SAI Number``
            ; stringValue       "site_name"         row.``Site Common Name``
            ; stringValue       "site_type"         row.``Site Type``
            ; stringValue       "discharge_name"    row.``Discharge Name``
            ]


// ***** Run inserts...

let runSQL (genSql:'a -> string) (rows:'a list)  : Script<int> = 
    let insertProc (row:'a) : SQLiteConn<int> = execNonQuery <| genSql row
    liftSQLiteConn <| withTransactionListSum rows insertProc

let insertAllDischarges () : Script<int> =  
    runSQL makeDisINSERT <| getDischargesXls ()




let SetupDB () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\discharges\erDischargeDB.sqlite3"
    let deleteAll = deleteAllData () |> logScript (sprintf "%i rows deleted") 

    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| deleteAll

    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ insertAllDischarges ()         |> logScript (sprintf "%i discharges inserted")       
            ]