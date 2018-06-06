// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.ClosedXMLOutput

// For Seq.tail
open FSharpx.Collections

open ClosedXML

// ClosedXML - First Cell is (Row=1,Col=1)
type RowIx = int

type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet


type ClosedXMLOutput<'a> = 
    ClosedXMLOutput of (ClosedXMLSheet -> RowIx -> (RowIx * 'a))

let runClosedXMLOutput (ma:ClosedXMLOutput<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLOutput(f) -> snd <| f sheet 1


let inline private apply1 (ma : ClosedXMLOutput<'a>) (sheet:ClosedXMLSheet) (rowIx:RowIx) : (RowIx * 'a) = 
    let (ClosedXMLOutput f) = ma in f sheet rowIx

let inline private unitM (x:'a) : ClosedXMLOutput<'a> = 
    ClosedXMLOutput <| fun r s -> (s,x)

let inline private bindM (ma:ClosedXMLOutput<'a>) (f : 'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<'b> =
    ClosedXMLOutput <| fun r s -> 
        let (s1,a) = apply1 ma r s in apply1 (f a) r s1

let fail : ClosedXMLOutput<'a> = 
    ClosedXMLOutput (fun r s -> failwith "ClosedXMLOutput fail")

type ClosedXMLOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let closedXMLOutput:ClosedXMLOutputBuilder = new ClosedXMLOutputBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:ClosedXMLOutput<'a>) : ClosedXMLOutput<'b> = 
    ClosedXMLOutput <| fun sheet rowIx ->
        let (s1,ans) = apply1 ma sheet rowIx in (s1, fn ans)


let mapM (fn: 'a -> ClosedXMLOutput<'b>) (xs: 'a list) : ClosedXMLOutput<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<'b list> = mapM fn xs

let mapMz (fn: 'a -> ClosedXMLOutput<'b>) (xs: 'a list) : ClosedXMLOutput<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<unit> = mapMz fn xs

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

let traverseM (fn: 'a -> ClosedXMLOutput<'b>) (source:seq<'a>) : ClosedXMLOutput<seq<'b>> = 
    ClosedXMLOutput <| fun sheet pos ->
        seqMapAccumL (fun st x -> apply1 (fn x) sheet st) pos source

let traverseMz (fn: 'a -> ClosedXMLOutput<'b>) (source:seq<'a>) : ClosedXMLOutput<unit> = 
    ClosedXMLOutput <| fun sheet pos ->
        let (s1,_) = seqMapAccumL (fun st x -> apply1 (fn x) sheet st) pos source in (s1,())

let traverseiM (fn:int -> 'a -> ClosedXMLOutput<'b>) (source:seq<'a>) : ClosedXMLOutput<seq<'b>> = 
    let finish = fun ((pos,_), ans) -> (pos,ans)
    ClosedXMLOutput <| fun sheet pos ->
        finish <| seqMapAccumL (fun (cursor,ix) x -> 
                                let (cursor1,ans) = apply1 (fn ix x) sheet cursor in ((cursor1,ix+1),ans)) (pos,0) source

let traverseiMz (fn:int -> 'a -> ClosedXMLOutput<'b>) (source:seq<'a>) : ClosedXMLOutput<unit> = 
    let finish = fun ((pos,_), _) -> (pos,())
    ClosedXMLOutput <| fun sheet pos ->
        finish <| seqMapAccumL (fun (cursor,ix) x -> 
                                let (cursor1,ans) = apply1 (fn ix x) sheet cursor in ((cursor1,ix+1),ans)) (pos,0) source

let mapiM (fn:int -> 'a -> ClosedXMLOutput<'b>) (xs: 'a list) : ClosedXMLOutput<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn ix y) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let foriM (xs: 'a list) (fn:int -> 'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<'b list> = mapiM fn xs

let mapiMz (fn:int -> 'a -> ClosedXMLOutput<'b>) (xs: 'a list) : ClosedXMLOutput<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn ix y) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

let foriMz (xs: 'a list)  (fn:int -> 'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<unit> = mapiMz fn xs

// ClosedXMLOutput-specific operations

type ClosedXMLOptions = 
    { SheetName: string }


let outputToNew (options:ClosedXMLOptions) (fileName:string) (ma:ClosedXMLOutput<'a>) : 'a =
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add(options.SheetName)
    let ans = runClosedXMLOutput ma outputsheet
    outputbook.SaveAs(fileName)
    ans


// Having whole row writing rather than cell writing as the primitive writing 
// operation makes the implementation simpler (state only needs to track the
// row number).

// This is the primitive Row writer, user code is expected to use a higher level interface.
let tellRowObjs (values:obj list) : ClosedXMLOutput<unit> =
    ClosedXMLOutput <| fun sheet rowIx ->
        List.iteri (fun ix value -> 
                        sheet.Cell(rowIx, ix+1).Value <- value) values
        (rowIx + 1, ())


// This will fail if it is not the first writer action.
let tellHeaders (values:string list) : ClosedXMLOutput<unit> =
    let proc1 = tellRowObjs <| List.map (fun s -> s :> obj) values
    ClosedXMLOutput <| fun sheet rowIx ->
        if rowIx = 1 then
            apply1 proc1 sheet rowIx
        else failwith "tellHeaders - not at first cell (something written already)"


// Design note - previous CellWriter had a phantom type paramater
// but as CellWriter was never a monad it wasn't actually useful.


type CellWriter = private Wrapped of obj
type RowWriter = CellWriter list

let private getWrapped (cellWriter:CellWriter) : obj = 
    match cellWriter with | Wrapped o -> o


let tellRow (rowWriter:RowWriter) : ClosedXMLOutput<unit> =
    tellRowObjs <| List.map getWrapped rowWriter
    
let tellRows (rowWriters:seq<RowWriter>) : ClosedXMLOutput<unit> =
    traverseMz tellRow rowWriters

let tellRecord (a:'record) (writeProc:'record -> RowWriter) : ClosedXMLOutput<unit> =
    tellRow <| writeProc a

let tellRecords (records:seq<'a>) (writeProc:'a -> RowWriter) : ClosedXMLOutput<unit> = 
    traverseMz (tellRow << writeProc) records

let tellRecordsi (records:seq<'a>) (writeProc:int -> 'a -> RowWriter) : ClosedXMLOutput<unit> = 
    traverseiMz (fun a ix -> tellRow <| writeProc a ix) records


// Procedures prefixed write_ rather than tell_ are expected to be used to generate
// all the output in a file.


let writeRowsWithHeaders (headers:string list) (rows:seq<RowWriter>) : ClosedXMLOutput<unit> = 
    closedXMLOutput { 
        do! tellHeaders headers
        do! traverseMz tellRow rows }

let writeRecordsWithHeaders (headers:string list) (records:seq<'a>) (writeRow:'a -> RowWriter) : ClosedXMLOutput<unit> = 
    closedXMLOutput { 
        do! tellHeaders headers
        do! tellRecords records writeRow }


let writeRecordsWithHeadersi (headers:string list) (records:seq<'a>) (writeRow:int -> 'a -> RowWriter) : ClosedXMLOutput<unit> = 
    closedXMLOutput { 
        do! tellHeaders headers
        do! tellRecordsi records writeRow }


let tellObj (value:obj) : CellWriter = 
    Wrapped <| value

let tellBool (value:bool) : CellWriter = tellObj (value :> obj)
let tellDateTime (value:System.DateTime) : CellWriter = tellObj (value :> obj)
let tellDecimal (value:decimal) : CellWriter = tellObj (value :> obj)
let tellFloat (value:float) : CellWriter = tellObj (value :> obj)
let tellGuid (value:System.Guid) : CellWriter = tellObj (value :> obj)   
let tellInteger (value:int) : CellWriter = tellObj (value :> obj)
let tellInteger64 (value:int64) : CellWriter = tellObj (value :> obj)

let tellInt (value:int) : CellWriter = tellObj (value :> obj)
let tellInt64 (value:int64) : CellWriter = tellObj (value :> obj)


let tellString (value:string) : CellWriter = 
    match value with 
    | null -> tellObj ("" :> obj)
    | _ -> tellObj (value :> obj)
    
// tellStringf would be nice but not sure how to achieve it...

