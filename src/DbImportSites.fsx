#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"

open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#load @"SQLiteUtils.fs"
open SQLiteUtils

#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"ClosedXMLWriter.fs"
open ClosedXMLWriter

type ImportTable = 
    ExcelFile< @"G:\work\Projects\rtu\import_data\ImportData.xlsx",
                SheetName = "SitesAndInstallations",
                ForceString = true >

type ImportRow = ImportTable.Row

//  **** DB Import

let connString = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sai_refs.sqlite")
    sprintf "Data Source=%s;Version=3;" dbSrc


let test01 () = 
    let query1 = "DELETE FROM all_sites;"
    let deleteProc = execNonQuery query1
    runSQLiteConn deleteProc connString

let test02 () : unit = 
    let query1 : string = "SELECT * FROM all_sites"
    let readProc (reader : SQLiteDataReader) = 
        while reader.Read() do
            printf "%s '%s'\n" (reader.GetString(0)) (reader.GetString(1)) 
    let proc = execReader query1 readProc
    runSQLiteConn proc connString

let test03 () = 
    let query1 : string = "INSERT INTO all_sites (sainum, sitename) VALUES ('SAI0000TEST', 'NAME/TEST');"
    let insertProc = withTransaction <| execNonQuery query1
    runSQLiteConn insertProc connString

let makeInsertQuery (row:ImportRow) : string =
    sprintf "INSERT INTO all_sites (sainum, installation_name, location_name, full_address, grid_ref) VALUES ('%s','%s','%s','%s','%s');"
        (cleanseValue row.InstReference)
        (cleanseValue row.InstCommonName)
        (cleanseValue row.SiteCommonName)
        (cleanseValue row.``Full Address``)
        (cleanseValue row.LocationReference)


let main () : unit = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.InstReference with null -> false | _ -> true
    let rows = importData.Data |> Seq.filter nullPred |> Seq.toList
    let rowProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| makeInsertQuery row
    let insertProc = withTransaction <| forMz rows rowProc
    runSQLiteConn insertProc connString


