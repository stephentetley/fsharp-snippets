module ClosedXMLWriter

// For Seq.tail
open FSharpx.Collections

open ClosedXML


type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet

type ClosedXMLWriter<'a> = 
    ClosedXMLWriter of (ClosedXMLSheet -> int -> (int * 'a))

let runClosedXMLWriter (ma:ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLWriter(f) -> snd <| f sheet 1


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

// This is the natural implementation for a traverseM with a state monad
let private seqMapAccumL (fn:'st -> 'a -> ('st * 'b)) (state:'st) (source:seq<'a>) : ('st * seq<'b>) = 
    let rec work (st:'st) (src:seq<'a>) = 
        if Seq.isEmpty src then (st, seq{ yield! [] })
        else 
            let a = Seq.head src
            let (st1,b) = fn st a
            let (st2,rest) = work st1 (Seq.tail src)
            (st2, seq { yield b; yield! rest })
    work state source

let traverseM (fn: 'a -> ClosedXMLWriter<'b>) (source:seq<'a>) : ClosedXMLWriter<seq<'b>> = 
    ClosedXMLWriter <| fun sheet rowIx ->
        seqMapAccumL (fun st x -> apply1 (fn x) sheet st) rowIx source

let traverseMz (fn: 'a -> ClosedXMLWriter<'b>) (source:seq<'a>) : ClosedXMLWriter<unit> = 
    ClosedXMLWriter <| fun sheet rowIx ->
        let (s1,_) = seqMapAccumL (fun st x -> apply1 (fn x) sheet st) rowIx source in (s1,())


let mapiM (fn: 'a -> int -> ClosedXMLWriter<'b>) (xs: 'a list) : ClosedXMLWriter<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let foriM (xs: 'a list) (fn: 'a -> int -> ClosedXMLWriter<'b>) : ClosedXMLWriter<'b list> = mapiM fn xs

let mapiMz (fn: 'a -> int -> ClosedXMLWriter<'b>) (xs: 'a list) : ClosedXMLWriter<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

let foriMz (xs: 'a list)  (fn: 'a -> int -> ClosedXMLWriter<'b>) : ClosedXMLWriter<unit> = mapiMz fn xs

// ClosedXMLWriter-specific operations

let outputToNew (ma:ClosedXMLWriter<'a>) (fileName:string) (sheetName:string) : 'a =
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add(sheetName)
    let ans = runClosedXMLWriter ma outputsheet
    outputbook.SaveAs(fileName)
    ans

let tellRow (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        List.iteri (fun ix e -> sheet.Cell(rowIx,ix+1).Value <- e) values
        (rowIx+1, ())


let tellHeaders (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        if rowIx = 1 then
            apply1 (tellRow values) sheet rowIx
        else failwith "tellHeaders - not first row"



// Experiment to make client code less stringy more "typeful"....

type CellWriter<'a> = int -> ClosedXMLWriter<'a>

let tellRow2 (valueProcs:(CellWriter<unit>) list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet rowIx ->
        ignore <| apply1 (mapiMz (fun proc colIx -> proc (colIx+1)) valueProcs) sheet rowIx
        (rowIx+1, ())


let tellObj (value:obj) : CellWriter<unit> = 
    fun colIx -> ClosedXMLWriter <| fun sheet rowIx -> sheet.Cell(rowIx,colIx+1).Value <- value; (rowIx, ())

let tellString (value:string) : CellWriter<unit> = 
    fun colIx -> ClosedXMLWriter <| fun sheet rowIx -> sheet.Cell(rowIx,colIx+1).Value <- value; (rowIx, ())

let tellInt (value:int) : CellWriter<unit> = 
    fun colIx -> ClosedXMLWriter <| fun sheet rowIx -> sheet.Cell(rowIx,colIx+1).Value <- value; (rowIx, ())
      
