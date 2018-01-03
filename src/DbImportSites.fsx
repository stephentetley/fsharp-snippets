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


type ImportTable = 
    ExcelFile< @"G:\work\Projects\rtu\import_data\ImportData.xlsx",
                SheetName = "SitesAndInstallations",
                ForceString = true >

type ImportRow = ImportTable.Row

//  **** DB Import

let connString = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sai_refs.sqlite")
    sprintf "Data Source=%s;Version=3;" dbSrc


let deleteData () : Result<int> = 
    let query1 = "DELETE FROM all_sites;"
    let deleteProc = execNonQuery query1
    runSQLiteConn deleteProc connString

let test02 () : unit = 
    let query1 : string = "SELECT * FROM all_sites"
    let readProc (reader : SQLiteDataReader) = 
        while reader.Read() do
            printf "%s '%s'\n" (reader.GetString(0)) (reader.GetString(1)) 
    let proc = execReader query1 readProc
    ignore <| runSQLiteConn proc connString


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


let main () : unit = 
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.InstReference with null -> false | _ -> true
    let rows = importData.Data |> Seq.filter nullPred |> Seq.toList
    let rowProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| genINSERT1 row

    // Ideally withTransaction would sum, but is this the correct thing to do? 
    let insertProc = withTransaction <| forM rows rowProc

    runResult (failwith) (printfn "Success: %A rows imported" << List.sum) <| resultMonad { 
        let! _ = deleteData ()
        let! ans = runSQLiteConn insertProc connString
        return ans
    }
    


