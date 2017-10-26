#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


let test01 () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    
    // Ideally accessing the filesys and accessing a named sheet should be guarded...
    let workbook : Excel.Workbook = app.Workbooks.Open(@"G:\work\working\data1.xlsx")
    let worksheet = workbook.Sheets.["DATA"] :?> Excel.Worksheet

    // Read a value
    let value = worksheet.Cells.Range("A1").Value2 :?> string
    printfn "%s" value
    
    // Find last cell
    let rng = worksheet.Cells.Find( What = "*",
                                    After = worksheet.Cells.Range("A1"),
                                    LookAt = Excel.XlLookAt.xlPart,
                                    LookIn = Excel.XlFindLookIn.xlFormulas,
                                    SearchOrder = Excel.XlSearchOrder.xlByRows,
                                    SearchDirection = Excel.XlSearchDirection.xlPrevious)
    printfn "LastRow: %d, LastCol: %d" rng.Rows.Row rng.Rows.Column

    workbook.Close(SaveChanges = false)
    app.Quit()

