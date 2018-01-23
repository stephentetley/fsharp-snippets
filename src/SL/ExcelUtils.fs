module SL.ExcelUtils

open System
open Microsoft.Office.Interop

open FSharp.Data

open SL.CommonUtils
open SL.AnswerMonad
open SL.ScriptMonad
open SL.CsvOutput
open SL.ClosedXMLOutput



let trimCsvFile (inputFile:string) (outputFile:string) (csvHasHeaders:bool) (sep:string) : unit =
    let truncRow (row:CsvRow) : SL.CsvOutput.CellWriter list = 
        Array.foldBack (fun (value:string) ac -> 
                         let a = value.Trim() |> SL.CsvOutput.tellString in a::ac) row.Columns [] 
        
    let csvRows : seq<CsvRow> = 
        CsvFile.Load(uri=inputFile, hasHeaders=csvHasHeaders, quote='"').Rows

    let procM : CsvOutput<unit> = 
        SL.CsvOutput.traverseMz (SL.CsvOutput.tellRow << truncRow) csvRows
        
    SL.CsvOutput.outputToNew {Separator=sep} procM outputFile    


// Output from Excel uses double quote and comma
let private csvTrimToClosedXML (inputFile:string) (outputFile:string) (sheetName:string) : unit =
    let truncRow (row:CsvRow) : SL.ClosedXMLOutput.CellWriter list = 
        Array.foldBack (fun (value:string) ac -> 
                         let a = value.Trim() |> tellString in a::ac) row.Columns [] 
        
    let csvRows : seq<CsvRow> = CsvFile.Load(uri=inputFile, hasHeaders=false, quote='"', separators=",").Rows
    let procM : ClosedXMLOutput<unit> = 
        SL.ClosedXMLOutput.traverseMz (SL.ClosedXMLOutput.tellRow << truncRow) csvRows
        
    SL.ClosedXMLOutput.outputToNew { SheetName = sheetName } procM outputFile 


type private ExcelScript<'a> = ScriptMonad<Excel.Application,'a>

let withExcel (fn:Excel.Application -> Answer<'a>) : ExcelScript<'a> = 
    scriptMonad.Bind(ask (), liftAnswer << fn)

// Outputs the first sheet to Csv, returns sheet name
let private xlsToCsv (inputFile:string) (outputFile:string) : ExcelScript<string> =
    withExcel <| fun app -> 
        try 
            // let app = new Excel.ApplicationClass(Visible = true) :> Excel.Application
            let book : Excel.Workbook = app.Workbooks.Open(inputFile)
            let sheet : Excel.Worksheet = book.Sheets.[1] :?> Excel.Worksheet
            let name : string = sheet.Name
            app.DisplayAlerts <- false      // Disable overwrite alert
            sheet.SaveAs(Filename = outputFile, FileFormat = Excel.XlFileFormat.xlCSV)
            app.DisplayAlerts <- true
            book.Close ()
            Ok <| name
        with
        | ex -> Err (ex.ToString())

let private csvToXls (inputFile:string) (outputFile:string) : ExcelScript<unit> =
    withExcel <| fun app -> 
        try 
            let book : Excel.Workbook = app.Workbooks.Open(inputFile)
            let sheet : Excel.Worksheet = book.Sheets.[1] :?> Excel.Worksheet
            let name : string = sheet.Name
            app.DisplayAlerts <- false      // Disable overwrite alert
            sheet.SaveAs(Filename = outputFile, FileFormat = Excel.XlFileFormat.xlOpenXMLWorkbook)
            app.DisplayAlerts <- true
            book.Close ()
            Ok ()
        with
        | ex -> Err (ex.ToString())

// TODO - delete temp files
let trimXlsSheet (inputFile:string) (outputFile:string) : unit = 
    let tempFile1 = IO.Path.ChangeExtension(outputFile, "csv")
    let tempFile2 = suffixFileName tempFile1 "-TRIM"
    let app = new Excel.ApplicationClass(Visible = true) :> Excel.Application
    try
        runAnswerWithError (consoleLogger) app <| 
            scriptMonad { 
                let! sheet = xlsToCsv inputFile tempFile1
                let! () = liftAction <| trimCsvFile tempFile1 tempFile2 false ","
                let! () = csvToXls tempFile2 outputFile
                return () }
    finally
        app.Quit()

        
let trimXlsFileToCsv (inputFile:string) (outputFile:string) : unit = 
    let tempFile = suffixFileName outputFile "-TEMP"
    let app = new Excel.ApplicationClass(Visible = true) :> Excel.Application
    try
        runAnswerWithError (consoleLogger) app <| 
            scriptMonad { 
                let! sheet = xlsToCsv inputFile tempFile
                let! _ = trimCsvFile tempFile outputFile false ","  |> liftAction
                return () }
    finally 
        app.Quit ()                               



// NOTE - this is largely obsolete.
// ClosedXML (and the ClosedXMLOutput monad) are much quicker for generating
// Excel output.

let WriteDummy (filename:string) (sheetname:string) : unit =
    let app = new Excel.ApplicationClass(Visible = true) 
    let (book:Excel.Workbook) = app.Workbooks.Add()
    let (sheet:Excel.Worksheet) = book.Worksheets.[1] :?> Excel.Worksheet
    sheet.Name <- sheetname
    // To disable overwrite alert
    app.DisplayAlerts <- false
    book.SaveAs(Filename = filename)
    app.DisplayAlerts <- true
    book.Close ()
    app.Quit()

let covertToCSV (inputFile:string) (outputFile:string) : unit =
    let app = new Excel.ApplicationClass(Visible = true) 
    let (book:Excel.Workbook) = app.Workbooks.Open(inputFile)
    app.DisplayAlerts <- false      // Disable overwrite alert
    book.SaveAs(Filename = outputFile, FileFormat = Excel.XlFileFormat.xlCSV)
    app.DisplayAlerts <- true
    book.Close ()
    app.Quit()

let covertToXlOpenXML (inputFile:string) (outputFile:string) : unit =
    let app = new Excel.ApplicationClass(Visible = true) 
    let (book:Excel.Workbook) = app.Workbooks.Open(inputFile)
    app.DisplayAlerts <- false      // Disable overwrite alert
    book.SaveAs(Filename = outputFile, FileFormat = Excel.XlFileFormat.xlOpenXMLWorkbook)
    app.DisplayAlerts <- true
    book.Close ()
    app.Quit()

let saveAndCloseWorkbook (workbook:Excel.Workbook) (filename:string) : unit =
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

// Don't use this.
// Use Cells.[i,j] for 2D index based addressing
let columnName (i:int) : string = 
    let rec work n ac = 
        if n > 0 then
            let remainder = n % 26
            if remainder = 0 then
                work ((n/26)-1) ('Z' :: ac)
            else 
                let ch = char (64+remainder)
                work (n/26) (ch :: ac) 
        else ac
    work i [] |> String.Concat

// Don't use this.
// Use Cells.[i,j] for 2D index based addressing
let addressName (rowIx:int) (colIx:int) : string = 
    sprintf "%s%d" (columnName colIx) rowIx



let findLastCell (worksheet:Excel.Worksheet) : (int * int) =
    let a1 = worksheet.Cells.Range("A1")
    let rng = worksheet.Cells.Find( What = "*",
                                    After = a1,
                                    LookAt = Excel.XlLookAt.xlPart,
                                    LookIn = Excel.XlFindLookIn.xlFormulas,
                                    SearchOrder = Excel.XlSearchOrder.xlByRows,
                                    SearchDirection = Excel.XlSearchDirection.xlPrevious)
    (rng.Rows.Row, rng.Rows.Column)

let findColumnCount (worksheet:Excel.Worksheet) : int =
    snd <| findLastCell worksheet

let findRowCount (worksheet:Excel.Worksheet) : int =
    fst <| findLastCell worksheet

