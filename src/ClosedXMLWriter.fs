module ClosedXMLWriter

// For Seq.tail
open FSharpx.Collections

open ClosedXML

// ClosedXML - First Cell is (Row=1,Col=1)
type Position = private { RowIx: int; ColIx: int}

type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet

type ClosedXMLWriter<'a> = 
    ClosedXMLWriter of (ClosedXMLSheet -> Position -> (Position * 'a))

let runClosedXMLWriter (ma:ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLWriter(f) -> snd <| f sheet {RowIx=1;ColIx=1}


let inline apply1 (ma : ClosedXMLWriter<'a>) (sheet:ClosedXMLSheet) (pos:Position) : (Position * 'a) = 
    let (ClosedXMLWriter f) = ma in f sheet pos

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
    ClosedXMLWriter <| fun sheet pos ->
        seqMapAccumL (fun st x -> apply1 (fn x) sheet st) pos source

let traverseMz (fn: 'a -> ClosedXMLWriter<'b>) (source:seq<'a>) : ClosedXMLWriter<unit> = 
    ClosedXMLWriter <| fun sheet pos ->
        let (s1,_) = seqMapAccumL (fun st x -> apply1 (fn x) sheet st) pos source in (s1,())

let traverseiM (fn: 'a -> int -> ClosedXMLWriter<'b>) (source:seq<'a>) : ClosedXMLWriter<seq<'b>> = 
    let finish = fun ((pos,_), ans) -> (pos,ans)
    ClosedXMLWriter <| fun sheet pos ->
        finish <| seqMapAccumL (fun (cursor,ix) x -> 
                                let (cursor1,ans) = apply1 (fn x ix) sheet cursor in ((cursor1,ix+1),ans)) (pos,0) source

let traverseiMz (fn: 'a -> int -> ClosedXMLWriter<'b>) (source:seq<'a>) : ClosedXMLWriter<unit> = 
    let finish = fun ((pos,_), _) -> (pos,())
    ClosedXMLWriter <| fun sheet pos ->
        finish <| seqMapAccumL (fun (cursor,ix) x -> 
                                let (cursor1,ans) = apply1 (fn x ix) sheet cursor in ((cursor1,ix+1),ans)) (pos,0) source

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

let private nextRow (pos:Position) : Position = {RowIx=pos.RowIx+1; ColIx=1}
let private incrCol (pos:Position) : Position = let cx = pos.ColIx in { pos with ColIx=cx+1}

let tellCell (value:obj) : ClosedXMLWriter<unit> = 
    ClosedXMLWriter <| fun sheet pos ->  
        sheet.Cell(pos.RowIx, pos.ColIx).Value <- value
        (incrCol pos, ())

let tellRow (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet pos ->
        ignore <| apply1 (mapMz tellCell values) sheet pos
        (nextRow pos, ())


let tellHeaders (values:string list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet pos ->
        if pos.RowIx = 1 && pos.ColIx = 1 then
            apply1 (tellRow values) sheet pos
        else failwith "tellHeaders - not at first cell (something written already)"



// Experiment to make client code less stringy and more "typeful"....
// There seem to be two nice interfaces - an Applicative-like chain with (*>) or 
// List<CellWriter<unit>>.  

type CellWriter<'a> = private Wrapped of ClosedXMLWriter<'a>
type RowWriter<'a> = CellWriter<'a> list

let private getWrapped (cellWriter:CellWriter<'a>) : ClosedXMLWriter<'a> = 
    match cellWriter with | Wrapped(fn) -> fn


let tellRow2 (valueProcs:(CellWriter<unit>) list) : ClosedXMLWriter<unit> =
    ClosedXMLWriter <| fun sheet pos ->
        ignore <| apply1 (mapMz getWrapped valueProcs) sheet pos
        (nextRow pos, ())

let tellRows2 (items:seq<'a>) (writeRow:'a -> CellWriter<unit> list) : ClosedXMLWriter<unit> = 
    traverseMz (tellRow2 << writeRow) items

let tellRowsi2 (items:seq<'a>) (writeRow:'a -> int -> CellWriter<unit> list) : ClosedXMLWriter<unit> = 
    traverseiMz (fun a ix -> tellRow2 <| writeRow a ix) items


let tellObj (value:obj) : CellWriter<unit> = 
    Wrapped <| tellCell obj

let tellString (value:string) : CellWriter<unit> = 
    match value with 
    | null -> tellObj ("" :> obj)
    | _ -> tellObj (value :> obj)

// tellStringf would be nice but not sure how to achieve it...

let tellInt (value:int) : CellWriter<unit> = tellObj (value :> obj)
let tellFloat (value:float) : CellWriter<unit> = tellObj (value :> obj)
      
