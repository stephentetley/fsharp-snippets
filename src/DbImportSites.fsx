﻿#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load @"ResultMonad.fs"
open ResultMonad
#load @"SqlUtils.fs"
open SqlUtils
#load @"SQLiteConn.fs"
open SQLiteConn


#load @"ScriptMonad.fs"
open ScriptMonad



type ImportTable = 
    ExcelFile< @"G:\work\Projects\rtu\import_data\ImportData.xlsx",
                SheetName = "SitesAndInstallations",
                ForceString = true >

type ImportRow = ImportTable.Row


type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

//  **** DB Import

let makeConnParams () : SQLiteConnParams = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sai_refs.sqlite")
    sqliteConnParamsVersion3 dbSrc


let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:SQLiteConnParams -> Result<'a>) : Script<'a> = 
    withConnParams <| (liftResult << fn)


let deleteData () : Script<int> = 
    liftWithConnParams <| runSQLiteConn (deleteAllRows "all_sites")


// This is the new style...
let genINSERT1 (row:ImportRow) : string = 
    sqlINSERT "all_sites" 
        <|  [ stringValue       "sainum"                row.InstReference
            ; stringValue       "installation_name"     row.InstCommonName
            ; stringValue       "location_name"         row.SiteCommonName
            ; stringValue       "postcode"              row.``Post Code``
            ; stringValue       "full_address"          row.``Full Address``
            ; stringValue       "grid_ref"              row.LocationReference
            ; stringValue       "ops_contact"           row.``Operational Responsibility``
            ; stringValue       "asset_type"            row.AssetType
            ]

let insertData (rows:seq<ImportRow>) : Script<int> = 
    let rowProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| genINSERT1 row
    liftWithConnParams <| runSQLiteConn (withTransactionSeqSum rows rowProc)

let main () : unit = 
    let conn = makeConnParams ()
    let nullPred (row:ImportRow) : bool = match row.InstReference with null -> false | _ -> true
    let rows = (new ImportTable()).Data |> Seq.filter nullPred
    
    runScript (failwith) (printfn "Success: %A rows imported") (consoleLogger) conn <| scriptMonad { 
        let! _   = deleteData ()
        let! ans = insertData rows
        return ans
    }
    


