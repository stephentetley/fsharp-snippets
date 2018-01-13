module SL.ExcelUtils

open System
open Microsoft.Office.Interop


// To go into a common module soon...
let suffixFileName (filePath:string) (suffix:string) = 
    let pathTo = IO.Path.GetDirectoryName filePath
    let ext = IO.Path.GetExtension filePath
    let justname = IO.Path.GetFileNameWithoutExtension filePath
    IO.Path.Combine (pathTo, sprintf "%s%s%s" justname suffix ext)


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

