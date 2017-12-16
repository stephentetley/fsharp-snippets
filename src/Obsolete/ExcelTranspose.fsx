#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

open System

#load @"..\ExcelUtils.fs"
open ExcelUtils

let cellIndex (col:string) (row:int) : string = 
    sprintf "%s%d" col row



let inputPath = @"G:\work\Projects\routers\transposition-table1.xlsx"
let outputPath = @"G:\work\Projects\routers\transpositions-output.xlsx"


let writeRow (worksheet:Excel.Worksheet) (rowIx:int) (values:string list) : unit = 
    let write1 (colIx:int)  (s:string) : int = 
        let rng1 = worksheet.Cells.[rowIx,colIx] :?> Excel.Range
        rng1.Value2 <- s
        colIx + 1
    ignore <| List.fold write1 1 values
  

let main () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let inputWorkbook : Excel.Workbook = app.Workbooks.Open(inputPath)
    let inputWorksheet = inputWorkbook.Sheets.["Sheet1"] :?> Excel.Worksheet
    let outputWorkbook : Excel.Workbook = app.Workbooks.Add()
    let outputWorksheet = outputWorkbook.Sheets.[1] :?> Excel.Worksheet
    for i in 2 .. 2 .. 1000 do
        let a1 = (inputWorksheet.Cells.[i,1] :?> Excel.Range).Value2 :?> string
        let b1 = (inputWorksheet.Cells.[i,2] :?> Excel.Range).Value2 :?> string
        let a2 = (inputWorksheet.Cells.[i+1,1] :?> Excel.Range).Value2 :?> string
        let b2 = (inputWorksheet.Cells.[i+1,2] :?> Excel.Range).Value2 :?> string
        printf "%i: %s ==> %s \n" i a1 b1
        writeRow outputWorksheet i [a1;b1;a2;b2]
        writeRow outputWorksheet (i+1) [a2;b2;a1;b1]
    saveAndCloseWorkbook outputWorkbook outputPath
    inputWorkbook.Close(SaveChanges = false)
    app.Quit()
