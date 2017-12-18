module ClosedXMLWriter

open ClosedXML

type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet

type ClosedXMLWriter<'a> = 
    ClosedXMLWriter of (ClosedXMLSheet -> int -> ('a * int))

let runClosedXMLWriter (ma:ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLWriter(f) -> fst <| f sheet 1

let outputToNew (ma:ClosedXMLWriter<'a>) (fileName:string) (sheetName:string) : 'a =
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add(sheetName)
    let ans = runClosedXMLWriter ma outputsheet
    outputbook.SaveAs(fileName)
    ans

let inline apply1 (ma : ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) (i:int) : ('a * int) = 
    let (ClosedXMLWriter f) = ma in f sheet i

let unit (x:'a) : ClosedXMLWriter<'a> = 
    ClosedXMLWriter <| fun r s -> (x,s)

let bind (ma:ClosedXMLWriter<'a>) (f : 'a -> ClosedXMLWriter<'b>) : ClosedXMLWriter<'b> =
    ClosedXMLWriter <| fun r s -> let (a,s1) = apply1 ma r s in apply1 (f a) r s1

let fail : ClosedXMLWriter<'a> = 
    ClosedXMLWriter (fun r s -> failwith "ClosedXMLWriter fail")

type ClosedXMLWriterBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = unit ()

let closedXMLWriter:ClosedXMLWriterBuilder = new ClosedXMLWriterBuilder()


let tellRow (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        List.iteri (fun ix e -> sheet.Cell(rowIx,ix+1).Value <- e) values
        ((), rowIx+1)


let tellHeaders (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        if rowIx = 1 then
            apply1 (tellRow values) sheet rowIx
        else failwith "tellHeaders - not first row"


let mapMz (fn: 'a -> ClosedXMLWriter<'b>) (xs: 'a list) : ClosedXMLWriter<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bind (fn y) (fun _ -> work ys)
        | [] -> unit ()
    work xs