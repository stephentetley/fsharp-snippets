#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


#load @"..\ExcelUtils.fs"
open ExcelUtils


// potentially use ClosedXML (GitHub, MIT License)

let readRow (worksheet:Excel.Worksheet) (count:int) (rowIx:int) : string[] =
    let c1:Excel.Range = worksheet.Cells.[rowIx,1] :?> Excel.Range
    let c2:Excel.Range = worksheet.Cells.[rowIx,count] :?> Excel.Range
    let rng : Excel.Range = worksheet.Range(c1,c2)
    [| for i in 1 .. count -> (rng.Cells.[1,i] :?> Excel.Range).Text :?> string |]

let trims (arr: string []) : obj [] = 
    Array.map (fun (s:string) -> (s.Trim()) :> obj) arr

let writeRow (worksheet:Excel.Worksheet) (rowIx:int) (arr:obj[]) : unit =
    let c1:Excel.Range = worksheet.Cells.[rowIx,1] :?> Excel.Range
    let c2:Excel.Range = worksheet.Cells.[rowIx,arr.Length] :?> Excel.Range
    let rng : Excel.Range = worksheet.Range(c1,c2)
    rng.Value2 <- arr

let test01 () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    // Ideally this should be guarded...
    let workbook : Excel.Workbook = app.Workbooks.Open(@"G:\work\RTS-WW-all-points.xlsx")
    
    // Ideally this should be guarded...
    let worksheet = workbook.Sheets.["Sheet1"] :?> Excel.Worksheet
    
    let value = worksheet.Cells.Range("A1").Value2 :?> string
    printfn "%s" (value.Trim ())
    let rng = worksheet.Cells.Find( What = "*",
                                    After = worksheet.Cells.Range("A1"),
                                    LookAt = Excel.XlLookAt.xlPart,
                                    LookIn = Excel.XlFindLookIn.xlFormulas,
                                    SearchOrder = Excel.XlSearchOrder.xlByRows,
                                    SearchDirection = Excel.XlSearchDirection.xlPrevious)
    printfn "LastRow: %d, LastCol: %d" rng.Rows.Row rng.Rows.Column


    workbook.Close(SaveChanges = false)
    app.Quit()

let inputFile = @"G:\work\Projects\T0975_EDM2\Kim.xlsx"
let outputFile = @"G:\work\Projects\T0975_EDM2\Kim-TRIMMED.xlsx"

let runIt () : unit = 
    // Run Excel as a visible application
    let (app : Excel.Application) = new Excel.ApplicationClass(Visible = true) :> Excel.Application
    // Ideally this should be guarded...
    let inputbook : Excel.Workbook = app.Workbooks.Open(inputFile)
    let outputbook : Excel.Workbook = app.Workbooks.Add()

    // Ideally this should be guarded...
    let inputsheet = inputbook.Sheets.["Sheet1"] :?> Excel.Worksheet
    let outputsheet = outputbook.Sheets.["Sheet1"] :?> Excel.Worksheet

    app.Calculation <- Excel.XlCalculation.xlCalculationManual
    app.EnableEvents <- false
    app.ScreenUpdating <- false
    let (rowCount,colCount) = findLastCell inputsheet
    for rowi in 1 .. 3000 do // rowCount do
        // let arr1 = trims <| readRow inputsheet colCount rowi
        let arr1 = Array.map (fun i -> sprintf "A0%i" i :> obj) [| 1 .. 14 |]
        writeRow outputsheet rowi arr1
        if (rowi % 100 = 1) then
            printfn "Row %i of %i" rowi rowCount
        else ()

    app.Calculation <- Excel.XlCalculation.xlCalculationAutomatic
    app.EnableEvents <- true
    app.ScreenUpdating <- true
    inputbook.Close(SaveChanges = false)
    saveAndCloseWorkbook outputbook outputFile    
    app.Quit()

let main () = runIt ()

