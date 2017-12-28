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
#load @"SQLiteConn.fs"
open SQLiteConn

#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"ClosedXMLWriter.fs"
open ClosedXMLWriter


let connString = 
    let dbSrc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data\sai_refs.sqlite")
    sprintf "Data Source=%s;Version=3;" dbSrc

let realName (s:string) : string = s.Replace('_','/').Trim()
let underscoreName (s:string) : string = s.Replace('/','_').Trim()

let findSAI (name:string) : string = 
    let query1 : string = 
        sprintf "SELECT sainum FROM all_sites WHERE sitename='%s';" (realName name)        
    let readProc (reader : SQLiteDataReader) = 
        if reader.Read() then reader.GetString(0) else ""
    match runSQLiteConn (execReader query1 readProc) connString with
    | Err(msg) -> failwith <| sprintf "Cannot finf SAI %s" name
    | Ok(a) -> a


let test04 () : string = findSAI "CUDWORTH/NO 2 STW"
    
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
            (findSAI row.sitename)
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

let makeOutputCells (row:WorkListRow) : string list = 
    [ realName row.sitename
    ; findSAI row.sitename
    ; "RTU Asset Replacement"
    ; "S3953"
    ; sprintf "%s S3953 RTU MMIM Upgrade Manual" (spaceName row.sitename)
    ; "O & M Manuals"
    ; "S3953"
    ; "1"
    ; sprintf "%s S3953 RTU Asset Replacement.pdf" (underscoreName row.sitename)
    ; dateStamp ()
    ; "" ]


let main () : unit = 
    let workData = new WorkListTable()
    let nullPred (row:WorkListRow) = match row.sitename with null -> false | _ -> true
    let rows : WorkListRow list = workData.Data |> Seq.filter nullPred |> Seq.toList
    let writerProc = closedXMLWriter {
        do! tellHeaders headers
        do! mapMz (tellRow << makeOutputCells) rows }

    ignore <| outputToNew writerProc xlsOutputPath "Sheet1"