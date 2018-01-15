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
#load @"SL\ResultMonad.fs"
open SL.ResultMonad
#load @"SL\SqlUtils.fs"
open SL.SqlUtils
#load @"SL\SQLiteConn.fs"
open SL.SQLiteConn

#load @"SL\ScriptMonad.fs"
open SL.ScriptMonad

#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper

// ********** DATA SETUP **********

type CatsPoweredTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Powered",
                ForceString = true >

type CatsPoweredRow = CatsPoweredTable.Row

let getCatsPoweredRows () : CatsPoweredRow list = 
    let dict : GetRowsDict<CatsPoweredTable, CatsPoweredRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsPoweredTable())


type CatsBatteryTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Battery",
                ForceString = true >

type CatsBatteryRow = CatsBatteryTable.Row

let getCatsBatteryRows () : CatsBatteryRow list = 
    let dict : GetRowsDict<CatsBatteryTable, CatsBatteryRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsBatteryTable())


type CatsNonTelemTable = 
    ExcelFile< @"G:\work\Projects\events2\CATS7-for-data.xlsx",
                SheetName = "Non_Telemetry",
                ForceString = true >

type CatsNonTelemRow = CatsNonTelemTable.Row

let getCatsNonTelemRows () : CatsNonTelemRow list = 
    let dict : GetRowsDict<CatsNonTelemTable, CatsNonTelemRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsNonTelemTable())



                
type StormDisPermitsTable = 
    ExcelFile< @"G:\work\Projects\events2\Storm-Dis-DEC2017.xlsx",
                SheetName = "Data",
                ForceString = true >

type StormDisPermitsRow = StormDisPermitsTable.Row

let getStormDisPermitsRows () : StormDisPermitsRow list = 
    let dict : GetRowsDict<StormDisPermitsTable, StormDisPermitsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Related AI Asset Name`` with null -> false | _ -> true }
    excelTableGetRows dict (new StormDisPermitsTable())

type SaiSitesTable = 
    ExcelFile< @"G:\work\Projects\events2\sai-data-dump.xlsx",
                SheetName = "Sai_Sites",
                ForceString = true >

type SaiSitesRow = SaiSitesTable.Row


let getSaiSitesRows () : SaiSitesRow list = 
    let dict : GetRowsDict<SaiSitesTable, SaiSitesRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.InstReference with null -> false | _ -> true }
    excelTableGetRows dict (new SaiSitesTable())


type OutstationsTable = 
    ExcelFile< @"G:\work\Projects\events2\rts-outstations-jan2018-TRIM.xlsx",
                SheetName = "Outstations",
                ForceString = true >

type OutstationRow = OutstationsTable.Row

let getOutstations () : OutstationRow list = 
    let dict : GetRowsDict<OutstationsTable, OutstationRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``OS name`` with null -> false | _ -> true }
    excelTableGetRows dict (new OutstationsTable())
// ********** SCRIPT **********
type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:SQLiteConnParams -> Result<'a>) : Script<'a> = 
    withConnParams <| (liftResult << fn)

//  **** DB Import

let deleteAllData () : Script<int> = 
    let proc = 
        SL.SQLiteConn.sumSequenceM [ deleteAllRows "cats_consents"
                                    ; deleteAllRows "storm_dis_permits" 
                                    ; deleteAllRows "sai_sites"
                                    ; deleteAllRows "rts_outstations" ]
    liftWithConnParams <| runSQLiteConn proc


// Unfortunately input speadsheet has three (almost) identical tables
let makePoweredCatConsentINSERT (row:CatsPoweredRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "POWERED"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]

let makeBatteryCatConsentINSERT (row:CatsBatteryRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "BATTERY"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]


let makeNonTelemCatConsentINSERT (row:CatsNonTelemRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "NO_TELEMETRY"
            ; stringValue       "asset_sai_number"      row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]


let makeStormDisPermitsINSERT (row:StormDisPermitsRow) : string =
    sqlINSERT "storm_dis_permits" 
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


let makeSaiSitesINSERT (row:SaiSitesRow) : string = 
    sqlINSERT "sai_sites" 
        <|  [ stringValue       "sai_number"            row.InstReference
            ; stringValue       "common_name"           row.InstCommonName 
            ; stringValue       "site_ngr"              row.LocationReference
            ; stringValue       "asset_type"            row.AssetType
            ; stringValue       "asset_status"          row.AssetStatus
            ; stringValue       "site_postcode"         row.``Post Code``
            ; stringValue       "site_address"          row.``Full Address``
            ; stringValue       "operational_contact"   row.``Operational Responsibility``
            ; stringValue       "work_centre"           row.``Work Centre``
            ; stringValue       "has_mains"             row.``Mains Electricity``
            ]

let makeOutstationINSERT (row:OutstationRow) : string = 
    sqlINSERT "rts_outstations" 
        <|  [ stringValue       "os_name"           row.``OS name``
            ; stringValue       "od_name"           row.``OD name``
            ; stringValue       "od_comment"        row.``OD comment``
            ; stringValue       "os_comment"        row.``OS comment``
            ; stringValue       "media"             row.Media
            ; stringValue       "os_addr"           row.``OS Addr``
            ; stringValue       "os_type"           row.``OS type``
            ]
let insertCatsConsents () : Script<int> = 
    let poweredRows = getCatsPoweredRows ()
    let poweredRowProc (row:CatsPoweredRow) : SQLiteConn<int> = execNonQuery <| makePoweredCatConsentINSERT row
    let batteryRows = getCatsBatteryRows ()
    let batteryRowProc (row:CatsBatteryRow) : SQLiteConn<int> = execNonQuery <| makeBatteryCatConsentINSERT row
    let nonTelemRows = getCatsNonTelemRows ()
    let nonTelemRowProc (row:CatsNonTelemRow) : SQLiteConn<int> = execNonQuery <| makeNonTelemCatConsentINSERT row
    let forMSum (xs:'a list) (proc:'a -> SQLiteConn<int>) = SL.SQLiteConn.fmapM (List.sum) <| SL.SQLiteConn.forM xs proc
    let combinedProc = 
        SL.SQLiteConn.sumSequenceM [ forMSum poweredRows poweredRowProc 
                                    ; forMSum batteryRows batteryRowProc 
                                    ; forMSum nonTelemRows nonTelemRowProc ]
    liftWithConnParams <| runSQLiteConn (withTransaction combinedProc)




let insertStormDisPermits () : Script<int> =  
    let rows = getStormDisPermitsRows ()
    let insertProc (row:StormDisPermitsRow) : SQLiteConn<int> = execNonQuery <| makeStormDisPermitsINSERT row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum rows insertProc)

let insertSaiSites () : Script<int> =  
    let rows = getSaiSitesRows ()
    let insertProc (row:SaiSitesRow) : SQLiteConn<int> = execNonQuery <| makeSaiSitesINSERT row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum rows insertProc)


let insertOutstations () : Script<int> =  
    let rows = getOutstations ()
    let insertProc (row:OutstationRow) : SQLiteConn<int> = execNonQuery <| makeOutstationINSERT row
    liftWithConnParams <| runSQLiteConn (withTransactionListSum rows insertProc)


let main () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\edmDB.sqlite3"
  
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ deleteAllData ()          |> logScript (sprintf "%i rows deleted") 
            ; insertCatsConsents ()     |> logScript (sprintf "%i cats_consents inserted")       
            ; insertStormDisPermits ()  |> logScript (sprintf "%i storm_dis_permits inserted")
            ; insertSaiSites ()         |> logScript (sprintf "%i sai_sites inserted") 
            ; insertOutstations ()      |> logScript (sprintf "%i rts_outstations inserted") 
            ]