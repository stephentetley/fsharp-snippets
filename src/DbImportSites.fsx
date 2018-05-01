#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net46"
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


type ImportTable = 
    ExcelFile< @"G:\work\Projects\rtu\import_data\ImportData.xlsx",
                SheetName = "SitesAndInstallations",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.InstReference with null -> false | _ -> true }

let getImportRows () : seq<ImportRow> = excelTableGetRowsSeq importTableDict (new ImportTable())


type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

//  **** DB Import

let makeConnParams () : SQLiteConnParams = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sites.sqlite")
    sqliteConnParamsVersion3 dbSrc


let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftSQLiteConn (sql:SQLiteConn<'a>) : Script<'a> = 
    withConnParams <| fun conn -> liftAnswer <| runSQLiteConn conn sql


let deleteData () : Script<int> = 
    liftSQLiteConn <| deleteAllRows "all_sites"


// This is the new style...
let genINSERT1 (row:ImportRow) : string = 
    sqlINSERT "all_sites" 
        <|  [ stringValue       "uid"                   row.InstReference
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
    liftSQLiteConn <| withTransactionSeqSum rows rowProc

let main () : unit = 
    let conn = makeConnParams ()
    let rows = getImportRows ()
    
    runScript (failwith) (printfn "Success: %A rows imported") (consoleLogger) conn <| scriptMonad { 
        let! _   = logScript (sprintf "%i rows deleted")        <| deleteData ()
        let! ans = logScript (sprintf "%i row inserted")        <| insertData rows
        return ans
    }
    


