#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


type InputTable = ExcelFile< @"G:\work\Projects\routers\Batched-site-lists.xlsx",
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




let saveWorkbook (workbook:Excel.Workbook) (filename:string)  : unit =
    let folderName = System.IO.Path.GetDirectoryName(filename)
    if not <| System.IO.Directory.Exists (folderName) then
        ignore <| System.IO.Directory.CreateDirectory folderName
    else ()

    let app:Excel.Application = workbook.Application
    // To disable overwrite alert
    app.DisplayAlerts <- false
    workbook.SaveAs(Filename = filename)
    app.DisplayAlerts <- true
    workbook.Close(SaveChanges = false)

let writeCell (sheet:Excel.Worksheet) (cellindex:string) (value:string) : unit = 
    sheet.Cells.Range(cellindex).Value2 <- value

let writeKnownCells (sheet:Excel.Worksheet) (pairs:(string*string) list) : unit = 
    List.iter (fun (a,b) -> writeCell sheet a b) pairs

let makeCellInserts (rowi:InputRow) : (string*string) list =
    [ ("B2", rowi.``AI2 Name`` )
    ; ("D2", rowi.``SAI Number``)
    ; ("B4", rowi.``Site Address``)
    ; ("B5", rowi.``Post Code``)
    ; ("D7", rowi.``ASDL Provider``)
    ; ("B9", rowi.``ASDL Line Number``)
    ; ("B11", rowi.``Serial Number``)
    ; ("B13", rowi.``Router IP Address``)
    ; ("B19", rowi.``Paired with``)
    ; ("D19", rowi.``Paired with I.P``)
    ]


// Note filename should include number suffix e.g. (5) if specified...
let processInputLine (app:Excel.Application) (rowi:InputRow) : unit = 
    let outpath = outfileName rowi.``Batch Number`` rowi.``Colloquial name`` rowi.``Router IP Address``
    let inserts = makeCellInserts rowi
    let workbook : Excel.Workbook = app.Workbooks.Open(templatepath)
    // let workbook = app.Workbooks.Item("data1.xlsx")
    let worksheet = workbook.Sheets.["Proforma"] :?> Excel.Worksheet
    writeKnownCells worksheet inserts
    saveWorkbook workbook outpath


let test01 () = 
    let outpath = outfileName "Batch1" @"KELDGATE/NO 3 WTW" "10.64.0.99"
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let workbook : Excel.Workbook = app.Workbooks.Open(templatepath)
    // let workbook = app.Workbooks.Item("data1.xlsx")
    let worksheet = workbook.Sheets.["Proforma"] :?> Excel.Worksheet
    writeKnownCells worksheet [ ("B2", "KELDGATE/NO 3 WTW")
                              ; ("D2", "SAI00163740")
                              ]
    saveWorkbook workbook outpath
    workbook.Close(SaveChanges = false)
    app.Quit()

let test02 () = 
    let file = new InputTable()
    for (rowi:InputRow) in file.Data do
       printfn "%s, %s, %s" rowi.``Colloquial name`` rowi.``Serial Number`` rowi.``AI2 Name``

let main () = 
    let file = new InputTable()
    let app = new Excel.ApplicationClass(Visible = true) 
    for (rowi:InputRow) in file.Data do
        match rowi.``AI2 Name`` with
        | null -> printfn "<finished>"
        | _ -> printfn "Processing %s..." rowi.``Colloquial name``
               processInputLine app rowi
    app.Quit()