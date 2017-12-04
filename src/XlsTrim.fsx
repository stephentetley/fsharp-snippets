#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


#load "ExcelUtils.fs"
open ExcelUtils


// Use cells
let trimCell (worksheet:Excel.Worksheet) (rowIx:int) (colIx:int) : unit =
    let rng1 = worksheet.Cells.[rowIx,colIx] :?> Excel.Range
    let ans = rng1.Text :?> string
    rng1.Value2 <- ans.Trim ()

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

let inputFile = @"G:\work\RTS-WW-all-points.xlsx"
let outputFile = @"G:\work\RTS-WW-all-points-TRIMMED.xlsx"

let runIt () : unit = 
    // Run Excel as a visible application
    let (app : Excel.Application) = new Excel.ApplicationClass(Visible = true) :> Excel.Application
    // Ideally this should be guarded...
    let workbook : Excel.Workbook = app.Workbooks.Open(inputFile)
    
    // Ideally this should be guarded...
    let worksheet = workbook.Sheets.["Sheet1"] :?> Excel.Worksheet
    
    app.Calculation <- Excel.XlCalculation.xlCalculationManual
    app.EnableEvents <- false
    app.ScreenUpdating <- false
    let (rowCount,colCount) = findLastCell worksheet
    for rowi in 1 .. rowCount do
        if rowi % 100 = 0 then
            printfn "Row %i of %i" rowi rowCount 
        else ()
        for colj in 1 .. colCount do
            trimCell worksheet rowi colj

    app.Calculation <- Excel.XlCalculation.xlCalculationAutomatic
    app.EnableEvents <- true
    app.ScreenUpdating <- true
    saveAndCloseWorkbook workbook outputFile    

let main () = runIt ()

