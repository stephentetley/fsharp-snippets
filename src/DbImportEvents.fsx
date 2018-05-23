#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net46"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames


#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\ExcelProviderHelper.fs"
// #load @"SL\Coord.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.ExcelProviderHelper
// open SL.Geo.Coord


// ********** DATA SETUP **********

type CatsPoweredTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\CATS7-for-data.xlsx",
                SheetName = "Powered",
                ForceString = true >

type CatsPoweredRow = CatsPoweredTable.Row

let getCatsPoweredRows () : CatsPoweredRow list = 
    let dict : GetRowsDict<CatsPoweredTable, CatsPoweredRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsPoweredTable())


type CatsBatteryTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\CATS7-for-data.xlsx",
                SheetName = "Battery",
                ForceString = true >

type CatsBatteryRow = CatsBatteryTable.Row

let getCatsBatteryRows () : CatsBatteryRow list = 
    let dict : GetRowsDict<CatsBatteryTable, CatsBatteryRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsBatteryTable())


type CatsNonTelemTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\CATS7-for-data.xlsx",
                SheetName = "Non_Telemetry",
                ForceString = true >

type CatsNonTelemRow = CatsNonTelemTable.Row

let getCatsNonTelemRows () : CatsNonTelemRow list = 
    let dict : GetRowsDict<CatsNonTelemTable, CatsNonTelemRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Permit Reference`` with null -> false | _ -> true }
    excelTableGetRows dict (new CatsNonTelemTable())



                
type StormDisPermitsTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\Storm-Dis-DEC2017.xlsx",
                SheetName = "Data",
                ForceString = true >

type StormDisPermitsRow = StormDisPermitsTable.Row

let getStormDisPermitsRows () : StormDisPermitsRow list = 
    let dict : GetRowsDict<StormDisPermitsTable, StormDisPermitsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``Related AI Asset Name`` with null -> false | _ -> true }
    excelTableGetRows dict (new StormDisPermitsTable())

type BaseSitesTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\adb-data-dump.xlsx",
                SheetName = "Base_Sites",
                ForceString = true >

type BaseSitesRow = BaseSitesTable.Row


let getBaseSitesRows () : BaseSitesRow list = 
    let dict : GetRowsDict<BaseSitesTable, BaseSitesRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.InstReference with null -> false | _ -> true }
    excelTableGetRows dict (new BaseSitesTable())


type OutstationsTable = 
    ExcelFile< @"G:\work\Projects\events2\db-import-tables\outstations-jan2018-TRIM.xlsx",
                SheetName = "OS",
                ForceString = true >

type OutstationRow = OutstationsTable.Row

let getOutstations () : OutstationRow list = 
    let dict : GetRowsDict<OutstationsTable, OutstationRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.``OS name`` with null -> false | _ -> true }
    excelTableGetRows dict (new OutstationsTable ())


type LotusData = 
    CsvProvider< @"G:\work\Projects\events2\db-import-tables\lnotes-secondary.csv",
                 HasHeaders = true>

type LotusRow = LotusData.Row

let getLotusContents () : LotusRow list = (new LotusData ()).Rows |> Seq.toList


type OutfallData = 
    CsvProvider< @"G:\work\Projects\events2\db-import-tables\outlets.csv",
                 HasHeaders = true>

type OutfallRow = OutfallData.Row

let getOutfalls () : OutfallRow list = (new OutfallData ()).Rows |> Seq.toList


// ********** SCRIPT **********
type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftSQLiteConn (sql:SQLiteConn<'a>) : Script<'a> = 
    withConnParams <| fun conn -> liftAnswer <| runSQLiteConn conn sql


//  **** DB Import

let deleteAllData () : Script<int> = 
    let proc = 
        SL.SQLiteConn.sumSequenceM [  deleteAllRows "cats_consents"
                                    ; deleteAllRows "storm_dis_permits" 
                                    ; deleteAllRows "base_sites"
                                    ; deleteAllRows "rts_outstations"
                                    ; deleteAllRows "lotus_consents"
                                    ; deleteAllRows "outfalls"]
    liftSQLiteConn proc


// Unfortunately input speadsheet has three (almost) identical tables
let makePoweredCatConsentINSERT (row:CatsPoweredRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "POWERED"
            ; stringValue       "asset_uid"             row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]

let makeBatteryCatConsentINSERT (row:CatsBatteryRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "BATTERY"
            ; stringValue       "asset_uid"             row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]


let makeNonTelemCatConsentINSERT (row:CatsNonTelemRow) : string = 
    sqlINSERT "cats_consents" 
        <|  [ stringValue       "permit_ref"            row.``Permit Reference``
            ; stringValue       "toplevel_permit_ref"   row.``Top level permit number``
            ; stringValue       "work_category"         "NO_TELEMETRY"
            ; stringValue       "asset_uid"             row.``SAI Number``
            ; stringValue       "asset_name"            row.``Overflow Name`` 
            ; stringValue       "outlet_ngr"            row.``Outlet NGR on consent`` ]


let makeStormDisPermitsINSERT (row:StormDisPermitsRow) : string =
    sqlINSERT "storm_dis_permits" 
        <|  [ stringValue       "asset_uid"             row.``SAI of related asset``
            ; stringValue       "asset_name"            row.``Related AI Asset Name``
            ; stringValue       "outlet_grid_ref"       row.``Outlet NGR``
            ; stringValue       "monitoring_grid_ref"   row.``Effluent Monitoring point NGR``
            ; stringValue       "discharge_decription"  row.``Description of Discharge`` 
            ; stringValue       "permit_urn"            row.``URN (permit number and schedule)`` 
            ; stringValue       "other_ea_name"         row.``Other/ EA name if different`` 
            ; stringValue       "receiving_watercourse" row.``Receiving water/ environment``
            ; stringValue       "screen_type"           row.``Screen type ('1D' for bar, '2D' for mesh or 'None')``
            ]


let makeBaseSitesINSERT (row:BaseSitesRow) : string = 
    sqlINSERT "base_sites" 
        <|  [ stringValue       "site_uid"              row.InstReference
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

            
let makeLotusConsentINSERT (row:LotusRow) : string = 
    let gridref : string  = 
        match row.``Outfall NGRE``.HasValue, row.``Outfall NGRN``.HasValue with
        | true,true -> 
            let east    = 1.0<meter> * (float <| row.``Outfall NGRE``.Value)
            let north   = 1.0<meter> * (float <| row.``Outfall NGRN``.Value)
            showOSGB36Point <| { Easting = east; Northing = north }
        | _ ,_ -> null

    sqlINSERT "lotus_consents" 
        <|  [ stringValue       "common_name"           row.``Common Name``
            ; stringValue       "asset_uid"             row.``AIB Reference``
            ; intNullableValue  "outfall_easting"       row.``Outfall NGRE``
            ; intNullableValue  "outfall_northing"      row.``Outfall NGRN``
            ; stringValue       "outfall_osgb36"        gridref
            ; stringValue       "full_consent_name"     row.``Full Consent``
            ; stringValue       "short_consent_name"    row.``Consent Number``
            ]
            
let makeOutfallINSERT (row:OutfallRow) : string = 
    let gridref : string  = 
        let east    = 1.0<meter> * (float <| row.METREEASTING)
        let north   = 1.0<meter> * (float <| row.METRENORTHING)
        showOSGB36Point <| { Easting = east; Northing = north}

    sqlINSERT "outfalls" 
        <|  [ stringValue       "stc25_ref"             row.STC25_REF
            ; decimalValue      "easting"               row.METREEASTING
            ; decimalValue      "northing"              row.METRENORTHING
            ; stringValue       "osgb36_gridref"        gridref
            ; stringValue       "function_node"         row.FUNCTION_NODE
            ]


// ***** Run inserts...
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
    liftSQLiteConn <| withTransaction combinedProc




let insertStormDisPermits () : Script<int> =  
    let rows = getStormDisPermitsRows ()
    let insertProc (row:StormDisPermitsRow) : SQLiteConn<int> = execNonQuery <| makeStormDisPermitsINSERT row
    liftSQLiteConn <| withTransactionListSum rows insertProc

let insertBaseSites () : Script<int> =  
    let rows = getBaseSitesRows ()
    let insertProc (row:BaseSitesRow) : SQLiteConn<int> = execNonQuery <| makeBaseSitesINSERT row
    liftSQLiteConn <| withTransactionListSum rows insertProc


let insertOutstations () : Script<int> =  
    let rows = getOutstations ()
    let insertProc (row:OutstationRow) : SQLiteConn<int> = execNonQuery <| makeOutstationINSERT row
    liftSQLiteConn <| withTransactionListSum rows insertProc

let insertLotusConsents () : Script<int> =  
    let rows = getLotusContents ()
    let insertProc (row:LotusRow) : SQLiteConn<int> = execNonQuery <| makeLotusConsentINSERT row
    liftSQLiteConn <| withTransactionListSum rows insertProc

let insertOutfalls () : Script<int> =  
    let rows = getOutfalls ()
    let insertProc (row:OutfallRow) : SQLiteConn<int> = execNonQuery <| makeOutfallINSERT row
    liftSQLiteConn <| withTransactionListSum rows insertProc


let main () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\eventsDB.sqlite3"
  
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ deleteAllData ()          |> logScript (sprintf "%i rows deleted") 
            ; insertCatsConsents ()     |> logScript (sprintf "%i cats_consents inserted")       
            ; insertStormDisPermits ()  |> logScript (sprintf "%i storm_dis_permits inserted")
            ; insertBaseSites ()        |> logScript (sprintf "%i base_sites inserted") 
            ; insertOutstations ()      |> logScript (sprintf "%i outstations inserted") 
            ; insertLotusConsents ()    |> logScript (sprintf "%i lotus consents inserted") 
            ; insertOutfalls ()         |> logScript (sprintf "%i outfalls inserted") 
            ]