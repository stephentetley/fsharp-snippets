#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#load @"SqlUtils.fs"
open SqlUtils
#load @"SQLiteConn.fs"
open SQLiteConn

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


let deleteData () = 
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



let makeInsertPermit (row:ImportRow) : string =
    sprintf "INSERT INTO permits (%s) VALUES ('%s','%s','%s','%s');"
        "permit_ref, permit_number, site_name, sai_number"
        (cleanseValue row.``Permit Reference``)
        (cleanseValue row.``Top level permit number``)
        (cleanseValue row.``Overflow Name``)
        (cleanseValue row.``SAI Number``)

let makeInsertSite (row:ImportRow) : string =
    sprintf "INSERT INTO sites (%s) VALUES ('%s','%s');"
        "sai_number, site_name"
        (cleanseValue row.``SAI Number``)
        (cleanseValue row.``Overflow Name``)

let makeInsertIWPermit (row:IWRow) : string =
    sprintf "INSERT INTO iw_permits (%s) VALUES ('%s','%s','%s','%s','%s','%s');"
        "sai_number, asset_name, outlet_grid_ref, monitor_grid_ref, discharge_decription, permit_urn"
        (cleanseValue row.``SAI of related asset``)
        (cleanseValue row.``Related AI Asset Name``)
        (cleanseValue row.``Outlet NGR``)
        (cleanseValue row.``Effluent Monitoring point NGR``)
        (cleanseValue row.``Description of Discharge``)
        (cleanseValue row.``URN (permit number and schedule)``)

let insertPermits () : unit = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.``Overflow Name`` with null -> false | _ -> true
    let allrows = importData.Data |> Seq.filter nullPred |> Seq.toList
    let permitInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertPermit row
    let insertProc = withTransaction <| forMz allrows permitInsProc
    ignore <| runSQLiteConn insertProc connString


let insertSites () : unit = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.``Overflow Name`` with null -> false | _ -> true
    let distProc (row:ImportRow) : string = row.``SAI Number``
    let siterows = 
        importData.Data |> Seq.filter nullPred |> Seq.distinctBy distProc |> Seq.toList
    let siteInsProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertSite row
    let insertProc = withTransaction <| forMz siterows siteInsProc
    ignore <| runSQLiteConn insertProc connString


let insertIWPermits () : unit = 
    let iwData = new IWTable()
    let nullPred (row:IWRow) = match row.``Related AI Asset Name`` with null -> false | _ -> true
    let allrows = iwData.Data |> Seq.filter nullPred |> Seq.toList
    let iwInsProc (row:IWRow) : SQLiteConn<int> = execNonQuery <| makeInsertIWPermit row
    let insertProc = withTransaction <| forMz allrows iwInsProc
    ignore <| runSQLiteConn insertProc connString
