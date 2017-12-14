module ExcelUtils

open System
open Microsoft.Office.Interop

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

type RowWriter<'a> = RowWriter of (Excel.Worksheet -> int -> ('a * int))

let runRowWriter (ma:RowWriter<'a>) (sheet:Excel.Worksheet) : 'a =
    match ma with
    | RowWriter(f) -> fst <| f sheet 1

let outputToNew (ma:RowWriter<'a>) (filename:string) (sheetname:string) : 'a =
    let app = new Excel.ApplicationClass(Visible = true) 
    let (book:Excel.Workbook) = app.Workbooks.Add()
    let (sheet:Excel.Worksheet) = book.Worksheets.[1] :?> Excel.Worksheet
    sheet.Name <- sheetname
    // To disable overwrite alert
    app.DisplayAlerts <- false
    let ans = runRowWriter ma sheet
    book.SaveAs(Filename = filename)
    app.DisplayAlerts <- true
    book.Close ()
    app.Quit()
    ans

let inline apply1 (ma : RowWriter<'a>) (sheet:Excel.Worksheet) (i:int) : ('a * int) = 
    let (RowWriter f) = ma in f sheet i

let unit (x:'a) : RowWriter<'a> = 
    RowWriter (fun r s -> (x,s))

let bind (ma:RowWriter<'a>) (f : 'a -> RowWriter<'b>) : RowWriter<'b> =
    RowWriter (fun r s -> let (a,s1) = apply1 ma r s in apply1 (f a) r s1)

let fail : RowWriter<'a> = RowWriter (fun r s -> failwith "RowWriter fail")

type RowWriterBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = unit ()

let (rowWriter:RowWriterBuilder) = new RowWriterBuilder()


let tellRow (values:string list) : RowWriter<unit> =
    RowWriter (fun sheet colIx ->
        let writeCell rowIx cellVal = 
            let rng1 = sheet.Cells.[colIx,rowIx] :?> Excel.Range
            rng1.Value2 <- cellVal
        let _ = values |> List.iteri (fun ix e -> writeCell (ix+1) e)
        ((), colIx+1))

let iterM (action: 'a -> RowWriter<unit>) (list: 'a list) : RowWriter<unit> = 
    let rec go (ls: 'a list) : RowWriter<unit> = 
        match ls with
        | x :: xs -> rowWriter { do! action x
                                 do! go xs }
        | [] -> rowWriter.Return ()
    go list