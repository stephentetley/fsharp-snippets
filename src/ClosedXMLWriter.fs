module ClosedXMLWriter

open ClosedXML

type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet

type ClosedXMLWriter<'a> = 
    ClosedXMLWriter of (ClosedXMLSheet -> int -> (int * 'a))

let runClosedXMLWriter (ma:ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLWriter(f) -> snd <| f sheet 1

let outputToNew (ma:ClosedXMLWriter<'a>) (fileName:string) (sheetName:string) : 'a =
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add(sheetName)
    let ans = runClosedXMLWriter ma outputsheet
    outputbook.SaveAs(fileName)
    ans

let inline apply1 (ma : ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) (i:int) : (int * 'a) = 
    let (ClosedXMLWriter f) = ma in f sheet i

let private unitM (x:'a) : ClosedXMLWriter<'a> = 
    ClosedXMLWriter <| fun r s -> (s,x)

let private bindM (ma:ClosedXMLWriter<'a>) (f : 'a -> ClosedXMLWriter<'b>) : ClosedXMLWriter<'b> =
    ClosedXMLWriter <| fun r s -> 
        let (s1,a) = apply1 ma r s in apply1 (f a) r s1

let fail : ClosedXMLWriter<'a> = 
    ClosedXMLWriter (fun r s -> failwith "ClosedXMLWriter fail")

type ClosedXMLWriterBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let closedXMLWriter:ClosedXMLWriterBuilder = new ClosedXMLWriterBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:ClosedXMLWriter<'a>) : ClosedXMLWriter<'b> = 
    ClosedXMLWriter <| fun sheet rowIx ->
        let (s1,ans) = apply1 ma sheet rowIx in (s1, fn ans)


let mapM (fn: 'a -> ClosedXMLWriter<'b>) (xs: 'a list) : ClosedXMLWriter<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> ClosedXMLWriter<'b>) : ClosedXMLWriter<'b list> = mapM fn xs

let mapMz (fn: 'a -> ClosedXMLWriter<'b>) (xs: 'a list) : ClosedXMLWriter<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> ClosedXMLWriter<'b>) : ClosedXMLWriter<unit> = mapMz fn xs


// ClosedXMLWriter-specific operations

let tellRow (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        List.iteri (fun ix e -> sheet.Cell(rowIx,ix+1).Value <- e) values
        (rowIx+1, ())


let tellHeaders (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        if rowIx = 1 then
            apply1 (tellRow values) sheet rowIx
        else failwith "tellHeaders - not first row"


