#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
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

#load @"ExcelProviderHelper.fs"
open ExcelProviderHelper


type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\events-stws.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.``Overflow Name`` with null -> false | _ -> true }

let getImportRows () : ImportRow list = excelTableGetRows importTableDict (new ImportTable())

                
type IWTable = 
    ExcelFile< @"G:\work\Projects\events2\Storm-Dis-DEC2017.xlsx",
                SheetName = "Data",
                ForceString = true >

type IWRow = IWTable.Row

let iwTableDict : GetRowsDict<IWTable, IWRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.``Related AI Asset Name`` with null -> false | _ -> true }

let getIWRows () : IWRow list = excelTableGetRows iwTableDict (new IWTable())


type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:SQLiteConnParams -> Result<'a>) : Script<'a> = 
    withConnParams <| (liftResult << fn)

//  **** DB Import

let deleteAllData () : Script<int> = 
    let proc = sqliteConn 
                { let! a = deleteAllRows "iw_permits" 
                  let! b = deleteAllRows "sites" 
                  let! c = deleteAllRows "permits" 
                  return a + b + c }
    liftWithConnParams <| runSQLiteConn proc



let makeInsertPermitStmt (row:ImportRow) : string =
    sqlINSERT "permits" 
        <|  [ stringValue       "permit_ref"        row.``Permit Reference``
            ; stringValue       "permit_number"     row.``Top level permit number``
            ; stringValue       "site_name"         row.``Overflow Name``
            ; stringValue       "sai_number"        row.``SAI Number``]


let makeInsertSite (row:ImportRow) : string =
    sqlINSERT "sites" 
        <|  [ stringValue       "sai_number"        row.``SAI Number``
            ; stringValue       "site_name"         row.``Overflow Name`` ]


let makeInsertIWPermit (row:IWRow) : string =
    sqlINSERT "iw_permits" 
        <|  [ stringValue       "sai_number"            row.``SAI of related asset``
            ; stringValue       "asset_name"            row.``Related AI Asset Name``
            ; stringValue       "outlet_grid_ref"       row.``Outlet NGR``
            ; stringValue       "monitor_grid_ref"      row.``Effluent Monitoring point NGR``
            ; stringValue       "discharge_decription"  row.``Description of Discharge`` 
            ; stringValue       "permit_urn"            row.``URN (permit number and schedule)``  ]


let insertPermits () : Script<int> = 
    let importRows = getImportRows () 
    let permitInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertPermitStmt row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum importRows permitInsProc)
    

// A site may have multiple rows - use List.distinctBy ...
let insertSites () : Script<int> = 
    let siteRows = getImportRows () |> List.distinctBy (fun row -> row.``SAI Number``)
    let siteInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertSite row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum siteRows siteInsProc)


let insertIWPermits () : Script<int> =  
    let allrows = getIWRows ()
    let iwInsProc (row:IWRow) : SQLiteConn<int> = execNonQuery <| makeInsertIWPermit row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum allrows iwInsProc)


let main () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\edmDB.sqlite3"
  
    runScript (failwith) (printfn "Success: %A rows imported") (consoleLogger) conn <| scriptMonad { 
        let! _ = logScript (sprintf "%i rows deleted")          <| deleteAllData ()
        let! a = logScript (sprintf "%i sites inserted")        <| insertSites ()
        let! b = logScript (sprintf "%i permits inserted")      <| insertPermits () 
        let! c = logScript (sprintf "%i iw_permits inserted")   <| insertIWPermits ()
        return (a+b+c)
    }
