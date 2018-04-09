#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\Coord.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.Geo.Coord

#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper



// ********** SCRIPT **********
type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftSQLiteConn (sql:SQLiteConn<'a>) : Script<'a> = 
    withConnParams <| fun conn -> liftAnswer <| runSQLiteConn conn sql


// ********** DATA SETUP **********

type InstallationsTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\Installations.xlsx",
                SheetName = "Installations",
                ForceString = true >

type InstallationsRow = InstallationsTable.Row

let getInstallations () : InstallationsRow list = 
    let dict : GetRowsDict<InstallationsTable, InstallationsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new InstallationsTable())

let getSites() : InstallationsRow list = 
    let projectSite (row:InstallationsRow) = row.SiteReference
    getInstallations () |> List.distinctBy projectSite


type ScreensTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-export-cat=SCREENS.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type ScreensRow = ScreensTable.Row


let getScreens () : ScreensRow list = 
    let dict : GetRowsDict<ScreensTable, ScreensRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new ScreensTable())

// DB-export-cat=RET_TANK
type RetTanksTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-export-cat=RET_TANK.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type RetTanksRow = RetTanksTable.Row


let getRetTanks () : RetTanksRow list = 
    let dict : GetRowsDict<RetTanksTable, RetTanksRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new RetTanksTable())

type SSTanksTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-export-cat=SS_TANK.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type SSTanksRow = SSTanksTable.Row


let getSSTanks () : SSTanksRow list = 
    let dict : GetRowsDict<SSTanksTable, SSTanksRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SSTanksTable())


type EsoAssetsTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-ESO-export.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type EsoAssetsRow = EsoAssetsTable.Row

let getEsoAssets () : EsoAssetsRow list = 
    let dict : GetRowsDict<EsoAssetsTable, EsoAssetsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new EsoAssetsTable())


type StwAssetsTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-STW-export.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type StwAssetsRow = StwAssetsTable.Row

let getStwAssets () : StwAssetsRow list = 
    let dict : GetRowsDict<StwAssetsTable, StwAssetsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new StwAssetsTable())

type SpsAssetsTable1 = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-SPS-export1.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type SpsAssetsRow1 = SpsAssetsTable1.Row

let getSpsAssets1 () : SpsAssetsRow1 list = 
    let dict : GetRowsDict<SpsAssetsTable1, SpsAssetsRow1> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SpsAssetsTable1())

type SpsExtras1Table = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-SPS-export2.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type SpsExtras1Row = SpsExtras1Table.Row

let getSpsExtras1 () : SpsExtras1Row list = 
    let dict : GetRowsDict<SpsExtras1Table, SpsExtras1Row> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SpsExtras1Table())


type CsoAssetsTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-CSO-export1.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type CsoAssetsRow = CsoAssetsTable.Row

let getCsoAssets () : CsoAssetsRow list = 
    let dict : GetRowsDict<CsoAssetsTable, CsoAssetsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new CsoAssetsTable())


type CsoExtras1Table = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-CSO-export2.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type CsoExtras1Row = CsoExtras1Table.Row

let getCsoExtras1 () : CsoExtras1Row list = 
    let dict : GetRowsDict<CsoExtras1Table, CsoExtras1Row> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new CsoExtras1Table())

type CsoExtras2Table = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-CSO-export3.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type CsoExtras2Row = CsoExtras2Table.Row

let getCsoExtras2 () : CsoExtras2Row list = 
    let dict : GetRowsDict<CsoExtras2Table, CsoExtras2Row> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new CsoExtras2Table())

type DtkAssetsTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-DTK-export1.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type DtkAssetsRow = DtkAssetsTable.Row

let getDtkAssets () : DtkAssetsRow list = 
    let dict : GetRowsDict<DtkAssetsTable, DtkAssetsRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new DtkAssetsTable())

type DtkExtras1Table = 
    ExcelFile< @"G:\work\Projects\events2\er-report\DB-DTK-export2.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type DtkExtras1Row = DtkExtras1Table.Row

let getDtkExtras1 () : DtkExtras1Row list = 
    let dict : GetRowsDict<DtkExtras1Table, DtkExtras1Row> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new DtkExtras1Table())


//  **** DB Import

let deleteAllData () : Script<int> = 
    let tables = 
        [ "all_assets"
        ; "all_sites"
        ; "screens"
        ; "ret_tanks"
        ; "ss_tanks"
        ; "eso_assets"
        ; "stw_assets"
        ; "sps_assets"
        ; "cso_assets"
        ; "dtk_assets"
        ]
    liftSQLiteConn  << SL.SQLiteConn.sumSequenceM <| List.map deleteAllRows tables


let makeAllSitesINSERT (row:InstallationsRow) : string = 
    sqlINSERT "all_sites" 
        <|  [ stringValue       "uid"               row.SiteReference
            ; stringValue       "site_name"         row.SiteCommonName
            ; stringValue       "site_address"      row.``Full Address``
            ; stringValue       "postcode"          row.``Post Code``
            ]

let makeAllAssetsINSERT (row:InstallationsRow) : string = 
    sqlINSERT "all_assets" 
        <|  [ stringValue       "uid"               row.InstReference
            ; stringValue       "asset_name"        row.InstCommonName
            ; stringValue       "grid_ref"          row.LocationReference
            ; stringValue       "parent_site_ref"   row.SiteReference
            ; stringValue       "asset_type"        row.AssetType
            ; stringValue       "asset_status"      row.AssetStatus
            ]

let makeScreensINSERT (row:ScreensRow) : string = 
    sqlINSERT "screens" 
        <|  [ stringValue       "uid"               row.Reference
            ; stringValue       "path_string"       row.``Common Name``
            ; stringValue       "manufacturer"      row.Manufacturer
            ; stringValue       "model"             row.Model
            ; stringValue       "screen_type"       row.``Screen Type``
            ; stringValue       "screen_aperture"   row.``Screen Aperture Size mm``
            ; stringValue       "screen_location"   row.``Location On Site``
            ; stringValue       "flow_units"        row.``Flow Units``
            ; stringValue       "flow"              row.Flow
            ; stringValue       "control_type"      row.``Automatic or Manual``
            ]

let makeRetTanksINSERT (row:RetTanksRow) : string = 
    sqlINSERT "ret_tanks" 
        <|  [ stringValue       "uid"               row.Reference
            ; stringValue       "path_string"       row.``Common Name``
            ]


let makeSSTanksINSERT (row:SSTanksRow) : string = 
    sqlINSERT "ss_tanks" 
        <|  [ stringValue       "uid"               row.Reference
            ; stringValue       "path_string"       row.``Common Name``
            ]


let makeEsoAssetsINSERT (row:EsoAssetsRow) : string = 
    sqlINSERT "eso_assets" 
        <|  [ stringValue       "uid"                   row.Reference
            ; stringValue       "common_name"           row.``Common Name``
            ; stringValue       "grid_ref"              row.``Loc.Ref.``
            ; stringValue       "op_status"             row.AssetStatus
            ; stringValue       "overflow_location"     row.``Location of Overflow``
            ; stringValue       "screens_type"          row.``Type of Screens``
            ; stringValue       "consent_ref"           row.``Consent Ref``
            ]

let makeStwAssetsINSERT (row:StwAssetsRow) : string = 
    sqlINSERT "stw_assets" 
        <|  [ stringValue       "uid"                   row.Reference
            ; stringValue       "common_name"           row.``Common Name``
            ; stringValue       "grid_ref"              row.``Loc.Ref.``
            ; stringValue       "op_status"             row.AssetStatus
            ; stringValue       "consent_ref"           row.``Consent Ref``
            ; stringValue       "process_type"          row.``Treatment Type``
            ]


let makeSpsAssetsINSERT (row:SpsAssetsRow1) : string = 
    sqlINSERT "sps_assets" 
        <|  [ stringValue       "uid"                   row.Reference
            ; stringValue       "common_name"           row.``Common Name``
            ; stringValue       "grid_ref"              row.``Loc.Ref.``
            ; stringValue       "op_status"             row.AssetStatus
            ; stringValue       "consent_ref"           row.``Consent Ref``
            ; stringValue       "consent_application"   row.``Consented For``
            ]

let makeSpsAssetsUPDATE1 (row:SpsExtras1Row) : string = 
    let updates = 
        [ stringValue       "eo_receiving_watercourse"      row.``EO Receiving Watercourse``
        ; stringValue       "eo_discharge_northing"         row.``EO Discharge Northing``
        ; stringValue       "eo_discharge_easting"          row.``EO Discharge Easting``
        ; stringValue       "function"                      row.Function
        ; stringValue       "overflow_grid_ref"             row.``Overflow NGR``
        ]
    let restrictions = 
        [ stringValue "uid"                   row.Reference   ]
    sqlUPDATE "sps_assets" updates restrictions


let makeCsoAssetsINSERT (row:CsoAssetsRow) : string = 
    sqlINSERT "cso_assets" 
        <|  [ stringValue       "uid"                   row.Reference
            ; stringValue       "common_name"           row.``Common Name``
            ; stringValue       "op_status"             row.AssetStatus
            ; stringValue       "grid_ref"              row.``Loc.Ref.``
            ; stringValue       "consent_ref"           row.``Consent Ref``
            ; stringValue       "consent_application"   row.``Consented For``
            ]


let makeCsoAssetsUPDATE1 (row:CsoExtras1Row) : string = 
    let updates = 
        [ stringValue       "overflow_location"         row.``Location of Overflow`` 
        ; stringValue       "overflow_weir_type"        row.``Overflow Weir Type``
        ; stringValue       "receiving_watercourse"     row.``Receiving Watercourse``
        ; stringValue       "pass_forward_flow"         row.``Pass Forward Flow l/s``
        ]
    let restrictions = 
        [ stringValue "uid"                   row.Reference   ]
    sqlUPDATE "cso_assets" updates restrictions
 
 
let makeCsoAssetsUPDATE2 (row:CsoExtras2Row) : string = 
    let updates = 
        [ stringValue       "screen_location"           row.``Screen location``
        ; stringValue       "screen_type"               row.``Type of Screens``
        ]
    let restrictions = 
        [ stringValue       "uid"                       row.Reference   ]
    sqlUPDATE "cso_assets" updates restrictions

let makeDtkAssetsINSERT (row:DtkAssetsRow) : string = 
    sqlINSERT "dtk_assets" 
        <|  [ stringValue       "uid"                   row.Reference
            ; stringValue       "common_name"           row.``Common Name``
            ; stringValue       "op_status"             row.AssetStatus
            ; stringValue       "grid_ref"              row.``Loc.Ref.``
            ; stringValue       "consent_ref"           row.``Consent Ref``
            ; stringValue       "consent_application"   row.``Consented For``
            ; stringValue       "function"              row.Function
            ]


let makeDtkAssetsUPDATE1 (row:DtkExtras1Row) : string = 
    let updates = 
        [ stringValue       "receiving_watercourse"     row.``Receiving Watercourse``
        ; stringValue       "screen_type"               row.``Type of Screens``
        ; stringValue       "screen_location"           row.``Screen location``
        ]
    let restrictions = 
        [ stringValue "uid"                   row.Reference   ]
    sqlUPDATE "dtk_assets" updates restrictions

// ***** Run inserts...

let runSQL (genSql:'a -> string) (rows:'a list)  : Script<int> = 
    let insertProc (row:'a) : SQLiteConn<int> = execNonQuery <| genSql row
    liftSQLiteConn <| withTransactionListSum rows insertProc

let insertAllSites () : Script<int> =  
    runSQL makeAllSitesINSERT <| getSites ()

let insertAllAssets () : Script<int> =
    runSQL makeAllAssetsINSERT <| getInstallations ()

    
let insertScreens () : Script<int> =  
    runSQL makeScreensINSERT <| getScreens ()
    
let insertRetTanks () : Script<int> = 
    runSQL makeRetTanksINSERT <| getRetTanks ()
    
let insertSSTanks () : Script<int> = 
    runSQL makeSSTanksINSERT <| getSSTanks ()
    
let insertEsoAssets () : Script<int> = 
    runSQL makeEsoAssetsINSERT <| getEsoAssets ()
    
let insertStwAssets () : Script<int> = 
    runSQL makeStwAssetsINSERT <| getStwAssets ()
    
let insertSpsAssets () : Script<int> = 
    runSQL makeSpsAssetsINSERT <| getSpsAssets1 ()

let updateSpsAssets1 () : Script<int> = 
    runSQL makeSpsAssetsUPDATE1 <| getSpsExtras1 ()
    
let insertCsoAssets () : Script<int> = 
    runSQL makeCsoAssetsINSERT <| getCsoAssets ()

let updateCsoAssets1 () : Script<int> = 
    runSQL makeCsoAssetsUPDATE1 <| getCsoExtras1 ()

let updateCsoAssets2 () : Script<int> = 
    runSQL makeCsoAssetsUPDATE2 <| getCsoExtras2 ()
    
let insertDtkAssets () : Script<int> = 
    runSQL makeDtkAssetsINSERT <| getDtkAssets ()

let updateDtkAssets1 () : Script<int> = 
    runSQL makeDtkAssetsUPDATE1 <| getDtkExtras1 ()

let SetupDB () : unit = 
    let conn = sqliteConnParamsVersion3  @"G:\work\Projects\events2\er-report\erReportDB.sqlite3"
    let deleteAll = deleteAllData () |> logScript (sprintf "%i rows deleted") 
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| deleteAll

    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ insertAllSites ()         |> logScript (sprintf "%i all_sites inserted")       
            ; insertAllAssets ()        |> logScript (sprintf "%i all_assets inserted") 
            ; insertScreens ()          |> logScript (sprintf "%i screens inserted") 
            ; insertRetTanks ()         |> logScript (sprintf "%i ret_tanks inserted")
            ; insertSSTanks ()          |> logScript (sprintf "%i ss_tanks inserted")
            ; insertEsoAssets ()        |> logScript (sprintf "%i eso_assets inserted")
            ; insertStwAssets ()        |> logScript (sprintf "%i stw_assets inserted")
            ; insertSpsAssets ()        |> logScript (sprintf "%i sps_assets inserted")
            ; updateSpsAssets1 ()       |> logScript (sprintf "%i sps_assets modified")
            ; insertCsoAssets ()        |> logScript (sprintf "%i cso_assets inserted")
            ; updateCsoAssets1 ()       |> logScript (sprintf "%i cso_assets modified")
            ; updateCsoAssets2 ()       |> logScript (sprintf "%i cso_assets modified")
            ; insertDtkAssets ()        |> logScript (sprintf "%i dtk_assets inserted")
            ; updateDtkAssets1 ()       |> logScript (sprintf "%i dtk_assets modified")
            ]