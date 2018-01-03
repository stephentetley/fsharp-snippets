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

type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\events-stws.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type ImportRow = ImportTable.Row

type IWTable = 
    ExcelFile< @"G:\work\Projects\events2\Storm-Dis-DEC2017.xlsx",
                SheetName = "Data",
                ForceString = true >

type IWRow = IWTable.Row


//  **** DB Import

let connString = 
    let dbloc = @"G:\work\Projects\events2\edmDB.sqlite3"
    sprintf "Data Source=%s;Version=3;" dbloc


let deleteData () : Result<int> = 
    let query1 = "DELETE FROM permits;"
    let deleteProc = execNonQuery query1
    runSQLiteConn deleteProc connString

let test02 () : unit = 
    let query1 : string = "SELECT * FROM permits"
    let readProc (reader : SQLiteDataReader) = 
        while reader.Read() do
            printf "%s '%s'\n" (reader.GetString(0)) (reader.GetString(1)) 
    let proc = execReader query1 readProc
    ignore <| runSQLiteConn proc connString



let genInsertPermitStmt (row:ImportRow) : string =
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


let insertPermits () : Result<int list> = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.``Overflow Name`` with null -> false | _ -> true
    let allrows = importData.Data |> Seq.filter nullPred |> Seq.toList
    let permitInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| genInsertPermitStmt row
    let insertProc = withTransactionList allrows permitInsProc
    runSQLiteConn insertProc connString
    

let insertSites () : unit = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.``Overflow Name`` with null -> false | _ -> true
    let distProc (row:ImportRow) : string = row.``SAI Number``
    let siterows = 
        importData.Data |> Seq.filter nullPred |> Seq.distinctBy distProc |> Seq.toList
    let siteInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertSite row
    let insertProc = withTransactionSeq siterows siteInsProc
    ignore <| runSQLiteConn insertProc connString


let insertIWPermits () : unit = 
    let iwData = new IWTable()
    let nullPred (row:IWRow) = match row.``Related AI Asset Name`` with null -> false | _ -> true
    let allrows = iwData.Data |> Seq.filter nullPred |> Seq.toList
    let iwInsProc (row:IWRow) : SQLiteConn<int> = execNonQuery <| makeInsertIWPermit row
    let insertProc = withTransactionSeq allrows iwInsProc
    ignore <| runSQLiteConn insertProc connString


  

