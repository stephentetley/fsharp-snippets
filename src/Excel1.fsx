
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


let test01 () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let workbook : Excel.Workbook = app.Workbooks.Open(@"G:\work\working\data1.xlsx")
    // let workbook = app.Workbooks.Item("data1.xlsx")
    let worksheet = workbook.Sheets.["DATA"] :?> Excel.Worksheet
    let value = worksheet.Cells.Range("A1").Text :?> string
    printfn "%s" value
    workbook.Close(SaveChanges = false)
    app.Quit()