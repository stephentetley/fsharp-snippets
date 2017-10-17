// Use ExcelProvider...
// This is nice but we cannot configure Schema like we can with the CsvProvider

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

type Data1Row = ExcelFile< @"G:\work\working\data1.xlsx",
                            SheetName = "DATA",
                            ForceString = true >

let test01 () = 
    let file = new Data1Row()
    let row = file.Data |> Seq.head
    for rowi in file.Data do
        printfn "%s %s %s" rowi.Reference rowi.``Installed From`` rowi.``Common Name``

    file.Data |> Seq.iter (fun d -> printfn "%s" d.Reference)
    printf "%s" row.AssetId
