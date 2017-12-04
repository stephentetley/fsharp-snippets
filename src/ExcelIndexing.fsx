#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "ExcelUtils.fs"
open ExcelUtils

type InputTable = ExcelFile< @"G:\work\Projects\routers\REVISED-site-lists.xlsx",
                             SheetName = "Site_List",
                             ForceString = true >

type InputRow = InputTable.Row

let templatepath = @"G:\work\Projects\routers\Router Commission Sample TEMPLATE.xlsx"

let safeName (input:string) : string = 
    let bads = ['\\'; '/'; ':'; '#']
    List.fold (fun s c -> s.Replace(c,'_')) input bads

let outfileName (batchname:string) (sitename:string) (ipaddr:string) : string =
    let s0 = safeName batchname
    let s1 = safeName sitename
    sprintf @"G:\work\Projects\routers\output\%s\Router Commission %s %s.xlsx" s0 s1 (safeName ipaddr)


let writeCell (sheet:Excel.Worksheet) (cellindex:string) (value:string) : unit = 
    sheet.Cells.Range(cellindex).Value2 <- value

let writeKnownCells (sheet:Excel.Worksheet) (pairs:(string*string) list) : unit = 
    List.iter (fun (a,b) -> writeCell sheet a b) pairs


let cellInsertsProforma (rowi:InputRow) : (string*string) list =
    [ ("B2", rowi.``Site Name`` )
    ; ("D2", rowi.``Site SAI Number``)
    ; ("B4", rowi.Address)
    ; ("B5", rowi.``Post Code``)
    ; ("D7", rowi.``ASDL Provider``)
    ; ("B9", rowi.``ADSL Line Number``)
    ; ("B11", rowi.``Serial Numbers``)
    ; ("B13", rowi.``Router IP``)
    ; ("B19", rowi.``Paired With``)
    ; ("D19", rowi.``Paired With IP Addr``)
    ]

let cellInsertsCheckList (rowi:InputRow) : (string*string) list =
    [ ("C33", rowi.``Private Wire`` )
    ]

// Note filename should include number suffix e.g. (5) if specified...
let processInputLine (app:Excel.Application) (rowi:InputRow) : unit = 
    let outpath = outfileName rowi.Batch rowi.``Colloquial Name`` rowi.``Router IP``
    // Some guards woul be nice...
    let workbook : Excel.Workbook = app.Workbooks.Open(templatepath)

    // Multiple sheets...
    let worksheetProforma = workbook.Sheets.["Proforma"] :?> Excel.Worksheet
    let insertsProforma = cellInsertsProforma rowi
    writeKnownCells worksheetProforma insertsProforma

    let worksheetCheckList = workbook.Sheets.["Check List"] :?> Excel.Worksheet
    let insertsCheckList = cellInsertsCheckList rowi
    writeKnownCells worksheetCheckList insertsCheckList

    saveAndCloseWorkbook workbook outpath


let main () = 
    let file = new InputTable()
    let app = new Excel.ApplicationClass(Visible = true) 
    for (rowi:InputRow) in file.Data do
        match rowi.``Site Name`` with
        | null -> printfn "<nullrow>"
        | _ -> printfn "Processing %s (%s)..." rowi.``Colloquial Name`` rowi.Batch
               processInputLine app rowi
    app.Quit()