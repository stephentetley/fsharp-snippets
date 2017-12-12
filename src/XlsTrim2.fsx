#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
open ClosedXML

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"C:\Windows\assembly\GAC_MSIL\office\15.0.0.0__71e9bce111e9429c"
#I @"C:\Windows\assembly\GAC_MSIL\Microsoft.Office.Interop.Access\15.0.0.0__71e9bce111e9429c"
open System.Data.OleDb

#load "ExcelUtils.fs"
open ExcelUtils




let test01 () : unit =
    let outputFile0 = @"G:\work\Projects\T0975_EDM2\closedxml.xlsx"
    let workbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let worksheet : ClosedXML.Excel.IXLWorksheet = workbook.Worksheets.Add("Sample Sheet")
    worksheet.Cell("A1").Value <- "Hello World!"
    workbook.SaveAs(outputFile0)

// OleDB is recommended as a fast way to process Xls files but it won't run on my machine.
let test02 () = 
    let con = @"Provider=Microsoft.ACE.OLEDB.12.0;Data Source=G:\work\Projects\T0975_EDM2\Kim.xlsx;" + 
              @"Extended Properties='Excel 8.0;HDR=Yes;'"
    use connection : OleDbConnection = new OleDbConnection(con)
    connection.Open();
    let command : OleDbCommand = new OleDbCommand("select * from [Sheet1$]", connection)
    use dreader : OleDbDataReader = command.ExecuteReader()
    while dreader.Read() do
        let row1col0 = dreader.[0] :?> string
        printfn "%s" row1col0




let inputFile = @"G:\work\Projects\T0975_EDM2\Kim.xlsx"
let outputFile = @"G:\work\Projects\T0975_EDM2\Kim-TRIMMED.xlsx"


let readRow (worksheet:Excel.Worksheet) (columnCount:int) (rowIx:int) : string[] =
    let c1:Excel.Range = worksheet.Cells.[rowIx,1] :?> Excel.Range
    let c2:Excel.Range = worksheet.Cells.[rowIx,columnCount] :?> Excel.Range
    let rng : Excel.Range = worksheet.Range(c1,c2)
    [| for i in 1 .. columnCount -> (rng.Cells.[1,i] :?> Excel.Range).Text :?> string |]

let trims (arr: string []) : obj [] = 
    Array.map (fun (s:string) -> (s.Trim()) :> obj) arr


let main () : unit = 
    // Run Excel as a visible application
    let (app : Excel.Application) = new Excel.ApplicationClass(Visible = true) :> Excel.Application
    // Ideally this should be guarded...
    let inputbook : Excel.Workbook = app.Workbooks.Open(inputFile)
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()

    // Ideally this should be guarded...
    let inputsheet = inputbook.Sheets.["Sheet1"] :?> Excel.Worksheet
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add("Sheet1")

    app.Calculation <- Excel.XlCalculation.xlCalculationManual
    app.EnableEvents <- false
    app.ScreenUpdating <- false

    let (rowCount,colCount) = findLastCell inputsheet
    for rowi in 1 .. 8000 do // rowCount do // rowCount do
        if (rowi % 100 = 1) then
            printfn "Row %i of %i" rowi rowCount
        else ()
        let vals = readRow inputsheet colCount rowi
        for coli in 1 .. colCount do
            // let val1 : string = (inputsheet.Cells.[rowi,coli] :?> Excel.Range).Text :?> string
            
            let val1 = sprintf "%i:%i" rowi coli
            outputsheet.Cell(rowi,coli).Value <- val1.Trim()

    app.Calculation <- Excel.XlCalculation.xlCalculationAutomatic
    app.EnableEvents <- true
    app.ScreenUpdating <- true
    inputbook.Close(SaveChanges = false)
    outputbook.SaveAs(outputFile)
    app.Quit()
