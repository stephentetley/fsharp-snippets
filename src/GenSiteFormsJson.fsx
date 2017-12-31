#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

#load "JsonOutput.fs"
open JsonOutput

let outputFile = @"G:\work\Projects\events2\survey-findreplace.json"

type InputTable = 
    ExcelFile< @"G:\work\Projects\events2\EDM2 Site-List.xlsx",
               SheetName = @"SITE_LIST",
               ForceString = true >

type InputRow = InputTable.Row

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
    let docname = sprintf "%s EDM2 Survey.docx" clean
    printfn "Clean is '%s'" clean
    let filename = System.IO.Path.Combine(clean,docname) 
    tellValue <| filename


let tellReplaces(row:InputRow) : JsonOutput<unit> = 
    tellObject  [ "#SITENAME",          tellString row.Name
                ; "#SAINUMBER",         tellString row.``SAI Number``
                ; "#SITEADDRESS",       tellString row.``Site Address``
                ; "#OPERSTATUS",        tellString ""
                ; "#SITEGRIDREF",       tellString row.``Site Grid Ref``
                ; "#ASSETTYPE",         tellString row.Type            
                ; "#OPERNAME",          tellString row.``Operational Responsibility`` 
                ; "#WORKCATEGORY",      tellString row.``Work Category``
                ; "#OUTFALLGRIDREF",    tellString row.``Outfall Grid Ref (from IW sheet)``
                ; "#RECWATERCOURSE",    tellString row.``Receiving Watercourse``
                ]

let tellRow1(row:InputRow) : JsonOutput<unit> = 
    printfn "%s" row.Name
    tellObject [ "FileName",  tellFileName row.Name
               ; "Replaces", tellReplaces row ]

let main () : unit = 
    let rows = readRows ()
    let proc = tellAsArray rows tellRow1
    ignore <| runJsonOutput proc 2 outputFile