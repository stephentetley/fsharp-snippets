module XlsWriter

open System
open Microsoft.Office.Interop


 // This should be obsolete - we should be able to use 
 // ClosedXMLOutput for batch writing and it is much faster...S
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

let unitM (x:'a) : RowWriter<'a> = 
    RowWriter (fun r s -> (x,s))

let bindM (ma:RowWriter<'a>) (f : 'a -> RowWriter<'b>) : RowWriter<'b> =
    RowWriter (fun r s -> let (a,s1) = apply1 ma r s in apply1 (f a) r s1)

let fail : RowWriter<'a> = RowWriter (fun r s -> failwith "RowWriter fail")

type RowWriterBuilder() = 
        member self.Return x = unitM x
        member self.Bind (p,f) = bindM p f
        member self.Zero () = unitM ()

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