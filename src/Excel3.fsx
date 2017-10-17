// Use ExcelProvider...
// This is nice but we cannot configure Schema like we can with the CsvProvider

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"

#load @"SheetWrite.fs"
open SheetWrite

type InputTable = ExcelFile< @"G:\work\working\name-trunc.xlsx",
                            SheetName = "NAME_TRUNC",
                            ForceString = true >

type InputRow = InputTable.Row


// TODO - could do this without exploding to an array...
let truncName (s:string) : string = 
    match s with
    | null -> ""
    | _ ->  let arr = s.Split [| '/' |]
            if arr.Length >= 2 then
                arr.[0] + "/" + arr.[1]
            else s


let demo01 () = 
    let file = new InputTable()
    for (rowi:InputRow) in file.Data do
       printfn "%s, %s, %s" rowi.``Reference`` (truncName <| rowi.``Common Name``) rowi.``Installed From`` 

let demo02 () = 
    truncName @"PILTDOWN/WTW/WATER SERVICES/WASHWATER PUMPING/YPV1411 PRESSURE VESSEL/EQUIPMENT: NITROGEN/WATER RECEIVER"

// TODO need a easy Spreadsheet writer (based on appending rows)
let demo03 () = 
    WriteDummy @"G:\work\working\output1.xlsx" "OUTPUT1"