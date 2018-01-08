#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

#load "JsonOutput.fs"
open JsonOutput

#load @"ExcelProviderHelper.fs"
open ExcelProviderHelper

let outputFile = @"G:\work\Projects\samps\cover-findreplace.json"

type InputTable = 
    ExcelFile< @"G:\work\Projects\samps\sitelist-for-gen-jan2018.xlsx",
               SheetName = @"Sheet1",
               ForceString = true >

type InputRow = InputTable.Row

let importTableDict : GetRowsDict<InputTable, InputRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.Site with null -> false | _ -> true }

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
    tellObject  [ "#SITENAME",      tellString row.Site
                ; "#SAINUM",        tellString row.Uid
                ]

let tellRow1(row:InputRow) : JsonOutput<unit> = 
    printfn "%s" row.Site
    tellObject [ "FileName",  tellFileName row.Site
               ; "Replaces", tellReplaces row ]

let main () : unit = 
    let rows = getImportRows () |> Seq.toList
    let proc = tellAsArray rows tellRow1
    ignore <| runJsonOutput proc 2 outputFile