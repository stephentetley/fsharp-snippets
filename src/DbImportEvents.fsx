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


type CatsPoweredTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Powered",
                ForceString = true >

type CatsPoweredRow = CatsPoweredTable.Row

let getCatsPoweredRows () : CatsPoweredRow list = 
    let catsPoweredDict : GetRowsDict<CatsPoweredTable, CatsPoweredRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows catsPoweredDict (new CatsPoweredTable())


type CatsBatteryTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Battery",
                ForceString = true >

type CatsBatteryRow = CatsBatteryTable.Row

let getCatsBatteryRows () : CatsBatteryRow list = 
    let catsBatteryDict : GetRowsDict<CatsBatteryTable, CatsBatteryRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows catsBatteryDict (new CatsBatteryTable())


type CatsNonTelemTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Non_Telemetry",
                ForceString = true >

type CatsNonTelemRow = CatsNonTelemTable.Row

let getCatsNonTelemRows () : CatsNonTelemRow list = 
    let odict : GetRowsDict<CatsNonTelemTable, CatsNonTelemRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows odict (new CatsNonTelemTable())



type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\events-stws.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type ImportRow = ImportTable.Row


let getImportRows () : ImportRow list = 
    let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Overflow Name`` with null -> false | _ -> true }
    excelTableGetRows importTableDict (new ImportTable())
                
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
    let proc = 
        SQLiteConn.fmapM (List.sum) 
            <| SQLiteConn.sequenceM [ deleteAllRows "cats_consents"
                                    ; deleteAllRows "iw_permits" 
                                    ; deleteAllRows "sites" 
                                    ; deleteAllRows "permits" ]
    liftWithConnParams <| runSQLiteConn proc

let makePoweredCatConsentStmt (row:CatsPoweredRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "POWERED"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` ]

let makeBatteryCatConsentStmt (row:CatsBatteryRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "BATTERY"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` ]


let makeNonTelemCatConsentStmt (row:CatsNonTelemRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "NO_TELEMETRY"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` ]

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
            ; stringValue       "monitoring_grid_ref"   row.``Effluent Monitoring point NGR``
            ; stringValue       "discharge_decription"  row.``Description of Discharge`` 
            ; stringValue       "permit_urn"            row.``URN (permit number and schedule)`` 
            ; stringValue       "other_ea_name"         row.``Other/ EA name if different`` 
            ; stringValue       "receiving_watercourse" row.``Receiving water/ environment``
            ; stringValue       "screen_type"           row.``Screen type ('1D' for bar, '2D' for mesh or 'None')``
            ]

let insertCatsConsents () : Script<int> = 
    let poweredRows = getCatsPoweredRows ()
    let poweredRowProc (row:CatsPoweredRow) : SQLiteConn<int> = execNonQuery <| makePoweredCatConsentStmt row
    let batteryRows = getCatsBatteryRows ()
    let batteryRowProc (row:CatsBatteryRow) : SQLiteConn<int> = execNonQuery <| makeBatteryCatConsentStmt row
    let nonTelemRows = getCatsNonTelemRows ()
    let nonTelemRowProc (row:CatsNonTelemRow) : SQLiteConn<int> = execNonQuery <| makeNonTelemCatConsentStmt row
    let forMSum (xs:'a list) (proc:'a -> SQLiteConn<int>) = SQLiteConn.fmapM (List.sum) <| SQLiteConn.forM xs proc
    let combinedProc = 
        SQLiteConn.fmapM (List.sum) 
            <| SQLiteConn.sequenceM 
                [ forMSum poweredRows poweredRowProc 
                ; forMSum batteryRows batteryRowProc 
                ; forMSum nonTelemRows nonTelemRowProc ]
    liftWithConnParams <| runSQLiteConn (withTransaction combinedProc)


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
        let! _ = logScript (sprintf "%i cats_consents inserted")        <| insertCatsConsents () 
        let! a = logScript (sprintf "%i sites inserted")        <| insertSites ()
        let! b = logScript (sprintf "%i permits inserted")      <| insertPermits () 
        let! c = logScript (sprintf "%i iw_permits inserted")   <| insertIWPermits ()
        return (a+b+c)
    }
