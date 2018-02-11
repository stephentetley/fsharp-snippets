﻿#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

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
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"


#load @"SL\AnswerMonad.fs"
#load @"SL\StringUtils.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\ClosedXMLOutput.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\ScriptMonad.fs"
open SL.AnswerMonad
open SL.StringUtils
open SL.SqlUtils
open SL.SQLiteConn
open SL.ClosedXMLOutput
open SL.ExcelProviderHelper
open SL.ScriptMonad





type ImportTable = 
    ExcelFile< @"G:\work\Projects\barriers\barriers_data.xlsx",
                SheetName = "INFO_ALL",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }

let getImportRows () : seq<ImportRow> = excelTableGetRowsSeq importTableDict (new ImportTable())


type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

//  **** DB Import

let makeConnParams () : SQLiteConnParams = 
    let dbSrc = @"G:\work\Projects\barriers\barrierDB.sqlite3"
    sqliteConnParamsVersion3 dbSrc


let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:SQLiteConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)


let deleteData () : Script<int> = 
    liftWithConnParams <| runSQLiteConn (deleteAllRows "installations")


// This is the new style...
let genINSERT1 (row:ImportRow) : string = 
    sqlINSERT "installations" 
        <|  [ stringValue       "asset_uid"         row.InstReference
            ; stringValue       "asset_name"        row.InstCommonName
            ; stringValue       "asset_status"      row.AssetStatus
            ; stringValue       "location"          row.LocationReference
            ]

let insertData (rows:seq<ImportRow>) : Script<int> = 
    let rowProc (row:ImportRow) : SQLiteConn<int> = execNonQuery <| genINSERT1 row
    liftWithConnParams <| runSQLiteConn (withTransactionSeqSum rows rowProc)

let main () : unit = 
    let conn = makeConnParams ()
    let rows = getImportRows ()
    
    runScript (failwith) (printfn "Success: %A rows imported") (consoleLogger) conn <| scriptMonad { 
        let! _   = logScript (sprintf "%i rows deleted")        <| deleteData ()
        let! ans = logScript (sprintf "%i row inserted")        <| insertData rows
        return ans
    }
    


let testZ () = 
    makeGlobPattern ["_"] [" "] "CHEVET CLIFFS_NO 2 LIGHTHOUSE"

let makeNameGlob (source:string) : string = makeGlobPattern ["_"] [" "] source
    

let genNameQuery (name:string) : string = 
    System.String.Format("""
        SELECT 
            asset_uid, asset_name, location
        FROM
            installations
        WHERE
            asset_name GLOB '{0}';
        """, makeNameGlob name)

type ResultRec = 
    { Uid: string
      Name: string
      Location: string } 

// This can probably have mutliple matches as the glob may be lax...
let nameGlobQuery (name:string) : SQLiteConn<ResultRec list> = 
    let query = genNameQuery name
    let procM (reader:SQLiteDataReader) : ResultRec = 
        { Uid = reader.GetString(0)
        ; Name = reader.GetString(1) 
        ; Location = reader.GetString(2)
        }
    execReaderList query procM          


let queryName1 (name:string) : Script<ResultRec list> = 
    liftWithConnParams <| runSQLiteConn (nameGlobQuery name)
    

let queryNames (names:string list) : Script<ResultRec list> = 
    SL.ScriptMonad.mapM queryName1 names |> SL.ScriptMonad.fmapM (List.concat >> List.distinct)

let exportSiteList (recs:ResultRec list) (xlsOutPath:string) : Script<unit> = 
    let proc1 (record:ResultRec) = 
        [ tellString record.Uid
        ; tellString record.Name 
        ; tellString record.Location
        ]
        
    liftAction <| 
        outputToNew { SheetName = "Sites" } 
                    (writeRecordsWithHeaders ["Uid"; "Name"; "Location"] recs proc1) 
                    xlsOutPath

let tempSiteList = 
    [ @"FOSTER ROAD"
    ; @"BURNLEY MOOR"
    ]

let genSiteList () : unit = 
    let conn = makeConnParams ()
    let outPath = @"G:\work\Projects\barriers\sites-list-barriers.xlsx"
    
    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn <| scriptMonad { 
        let! records = queryNames tempSiteList
        do! exportSiteList records outPath
    }

