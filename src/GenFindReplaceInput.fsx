#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\Newtonsoft.Json.11.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

#load @"SL\JsonOutput.fs"
open SL.JsonOutput

#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper

let outputFile = @"G:\work\Projects\samps\cover-findreplace.json"

type InputTable = 
    ExcelFile< @"G:\work\Projects\samps\UWW_Samps-sitelist.xlsx",
               SheetName = @"Site_List",
               ForceString = true >

type InputRow = InputTable.Row

let importTableDict : GetRowsDict<InputTable, InputRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.``Site Name`` with null -> false | _ -> true }

let getImportRows () : seq<InputRow> = excelTableGetRowsSeq importTableDict (new InputTable())



let safeName (input:string) : string = 
    let bads1 = ['\\'; '/'; ':']
    let bads2 = ["("; ")"; "["; "]"; "\n"]
    let input1 :string = List.fold (fun s c -> s.Replace(c,'_')) (input.Trim()) bads1
    List.fold (fun s c -> s.Replace(c,"")) input1 bads2

let readRows () : InputRow list = 
    let workData = new InputTable()
    let nullPred (row:InputRow) = match row.GetValue(0) with null -> false | _ -> true
    workData.Data |> Seq.filter nullPred |> Seq.toList

let tellFileName (siteName:string) : JsonOutput<unit> =
    let clean = siteName.Trim() |> safeName
    let docname = sprintf "%s UWW Samplers cover.docx" clean
    printfn "Clean is '%s'" clean
    let filename = System.IO.Path.Combine(clean,docname) 
    tellValue <| filename


let tellReplaces(row:InputRow) : JsonOutput<unit> = 
    tellObject  [ "#SITENAME",      tellString row.``Site Name``
                ; "#SAINUM",        tellString row.``SAI Ref``
                ]

let tellRow1(row:InputRow) : JsonOutput<unit> = 
    printfn "%s" row.``Site Name``
    tellObject [ "FileName",  tellFileName row.``Site Name``
               ; "Replaces", tellReplaces row ]

let main () : unit = 
    let rows = getImportRows () |> Seq.toList
    let proc = tellAsArray rows tellRow1
    ignore <| runJsonOutput {IndentLevel=2} outputFile proc