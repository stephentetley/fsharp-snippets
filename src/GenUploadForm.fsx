﻿#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net45"
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

#I @"..\packages\DocumentFormat.OpenXml.2.8.1\lib\net46"
#I @"..\packages\FastMember.Signed.1.3.0\lib\net45"
#I @"..\packages\ClosedXML.0.92.1\lib\net46"
#r "ClosedXML"


#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
open SL.AnswerMonad
open SL.SQLiteConn


#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput


let connParams = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sites.sqlite")
    sqliteConnParamsVersion3 dbSrc

let realName (s:string) : string = s.Replace('_','/').Trim()
let underscoreName (s:string) : string = s.Replace('/','_').Trim()

let findUid (name:string) : string = 
    let query1 : string = 
        sprintf "SELECT uid FROM all_sites WHERE sitename='%s';" (realName name)        
    let readProc (reader : SQLiteDataReader) = 
        if reader.Read() then reader.GetString(0) else ""
    match runSQLiteConn connParams (execReader query1 readProc) with
    | Err(msg) -> failwith <| sprintf "Cannot find Uid %s" name
    | Ok(a) -> a

    
type WorkListTable = 
    ExcelFile< @"G:\work\Projects\rtu\Final_Docs\year3-batch2-manuals-todo.xlsx",
                SheetName = "TO_MAKE",
                ForceString = true >

type WorkListRow = WorkListTable.Row

let spaceName (input:string) : string = 
    let bads = ['\\'; '/'; '_']
    List.fold (fun s c -> s.Replace(c,' ')) input bads


let makeOutputRow (row:WorkListRow) : string = 
    sprintf "%s,%s,RTU Asset Replacement,S3953,%s RTU MMIM Upgrade Manual,O & M Manuals,S3953,1,%s S3953 RTU Asset Replacement.pdf,<now>"
            (realName row.sitename)
            (findUid row.sitename)
            (spaceName row.sitename)
            (underscoreName row.sitename)

let xlsOutputPath = @"G:\work\Projects\rtu\Final_Docs\__edms_upload.xlsx"
let headers = [ "Asset Name";
                "Asset Reference";
                "Project Name";
                "Project Code";
                "Title";
                "Category";
                "Reference Number";
                "Revision";
                "Document Name";
                "Document Date";
                "Sheet/Volume" ]

let dateStamp () : string =
    System.DateTime.Now.ToString("dd/MM/yyyy")

let timeStamp () : string =
    System.DateTime.Now.ToString("HH:mm:ss")

let makeOutputCells (row:WorkListRow) : RowWriter = 
    [ tellString            <| realName row.sitename
    ; tellString            <| findUid row.sitename
    ; tellString            <| "RTU Asset Replacement"
    ; tellString            <| "S3953"
    ; tellString            <| sprintf "%s S3953 RTU MMIM Upgrade Manual" (spaceName row.sitename)
    ; tellString            <| "O & M Manuals"
    ; tellString            <| "S3953"
    ; tellInt               <| 1
    ; tellString            <| sprintf "%s S3953 RTU Asset Replacement.pdf" (underscoreName row.sitename)
    ; tellString            <| dateStamp ()
    ; tellString            <| "" 
    ]


let main () : unit = 
    let workData = new WorkListTable()
    let nullPred (row:WorkListRow) = match row.sitename with null -> false | _ -> true
    let rows : WorkListRow list = workData.Data |> Seq.filter nullPred |> Seq.toList
    let writerProc = closedXMLOutput {
        do! tellHeaders headers
        do! mapMz (fun a -> tellRow (makeOutputCells a)) rows }

    ignore <| outputToNew { SheetName = "Sheet1"} xlsOutputPath writerProc 