#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

open System

#load "ExcelUtils.fs"
open ExcelUtils

//// TODO - better to use Cells and 2D array indexing

let cellIndex (col:string) (row:int) : string = 
    sprintf "%s%d" col row

type Item = { Name : string; 
              SerialNumber : string;
              IPAddress: string;
              ADSLNumber: string;
              PairedWith : string;
              PairedWithIP : string }

let inputPath = @"G:\work\Projects\routers\IP_Subnets.xlsx"
let outputPath = @"G:\work\Projects\routers\IP_Subnets_merged.xlsx"



let writeRow (sheet:Excel.Worksheet) (rowindex:int) (value:Item) : unit = 
    sheet.Cells.Range(cellIndex "A" rowindex).Value2 <- value.Name
    sheet.Cells.Range(cellIndex "B" rowindex).Value2 <- value.SerialNumber
    sheet.Cells.Range(cellIndex "C" rowindex).Value2 <- value.IPAddress
    sheet.Cells.Range(cellIndex "D" rowindex).Value2 <- value.ADSLNumber
    sheet.Cells.Range(cellIndex "E" rowindex).Value2 <- value.PairedWith
    sheet.Cells.Range(cellIndex "F" rowindex).Value2 <- value.PairedWithIP

let test01 () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let inputWorkbook : Excel.Workbook = app.Workbooks.Open(inputPath)
    let inputWorksheet = inputWorkbook.Sheets.["IP_Subnets"] :?> Excel.Worksheet
    for i in 2 .. 2 .. 1000 do
        let ix = sprintf "A%d" i
        printf "%i: %s\n" i ((inputWorksheet.Cells.Range(ix).Value2) :?> string)
    app.Quit()

let main () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let inputWorkbook : Excel.Workbook = app.Workbooks.Open(inputPath)
    let inputWorksheet = inputWorkbook.Sheets.["IP_Subnets"] :?> Excel.Worksheet
    let outputWorkbook : Excel.Workbook = app.Workbooks.Add()
    let outputWorksheet = outputWorkbook.Sheets.[1] :?> Excel.Worksheet
    for i in 2 .. 2 .. 1000 do
        let item1 = 
            { Name = inputWorksheet.Cells.Range(cellIndex "A" i).Value2 :?> string; 
              SerialNumber = inputWorksheet.Cells.Range(cellIndex "B" i).Value2 :?> string; 
              IPAddress = inputWorksheet.Cells.Range(cellIndex "D" i).Value2 :?> string;
              ADSLNumber = inputWorksheet.Cells.Range(cellIndex "E" i).Value2 :?> string;
              PairedWith = inputWorksheet.Cells.Range(cellIndex "A" (i+1)).Value2 :?> string;
              PairedWithIP = inputWorksheet.Cells.Range(cellIndex "D" (i+1)).Value2 :?> string; }

        printf "%i: %s ==> %s \n" i item1.Name item1.PairedWith
        let item2 = 
            { Name = inputWorksheet.Cells.Range(cellIndex "A" (i+1)).Value2 :?> string; 
              SerialNumber = inputWorksheet.Cells.Range(cellIndex "B" (i+1)).Value2 :?> string; 
              IPAddress = inputWorksheet.Cells.Range(cellIndex "D" (i+1)).Value2 :?> string;
              ADSLNumber = inputWorksheet.Cells.Range(cellIndex "E" (i+1)).Value2 :?> string;
              PairedWith = inputWorksheet.Cells.Range(cellIndex "A" i).Value2 :?> string;
              PairedWithIP = inputWorksheet.Cells.Range(cellIndex "D" i).Value2 :?> string; }

        printf "%i: %s ==> %s \n" (i+1) item2.Name item2.PairedWith
        writeRow outputWorksheet i item1
        writeRow outputWorksheet (i+1) item2
    saveAndCloseWorkbook outputWorkbook outputPath
    inputWorkbook.Close(SaveChanges = false)
    app.Quit()
