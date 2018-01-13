module SL.ClosedXMLOutput

// For Seq.tail
open FSharpx.Collections

open ClosedXML

// ClosedXML - First Cell is (Row=1,Col=1)
type Position = private { RowIx: int; ColIx: int}

type ClosedXMLSheet = ClosedXML.Excel.IXLWorksheet


type ClosedXMLOutput<'a> = 
    ClosedXMLOutput of (ClosedXMLSheet -> Position -> (Position * 'a))

let runClosedXMLOutput (ma:ClosedXMLOutput<'a>) (sheet:ClosedXMLSheet) : 'a =
    match ma with
    | ClosedXMLOutput(f) -> snd <| f sheet {RowIx=1;ColIx=1}


let inline apply1 (ma : ClosedXMLOutput<'a>) (sheet:ClosedXMLSheet) (pos:Position) : (Position * 'a) = 
    let (ClosedXMLOutput f) = ma in f sheet pos

let private unitM (x:'a) : ClosedXMLOutput<'a> = 
    ClosedXMLOutput <| fun r s -> (s,x)

let private bindM (ma:ClosedXMLOutput<'a>) (f : 'a -> ClosedXMLOutput<'b>) : ClosedXMLOutput<'b> =
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

let outputToNew (ma:ClosedXMLOutput<'a>) (fileName:string) (sheetName:string) : 'a =
    let outputbook : ClosedXML.Excel.XLWorkbook = new ClosedXML.Excel.XLWorkbook()
    let outputsheet : ClosedXML.Excel.IXLWorksheet = outputbook.Worksheets.Add(sheetName)
    let ans = runClosedXMLOutput ma outputsheet
    outputbook.SaveAs(fileName)
    ans

let private nextRow (pos:Position) : Position = {RowIx=pos.RowIx+1; ColIx=1}
let private incrCol (pos:Position) : Position = let cx = pos.ColIx in { pos with ColIx=cx+1}

// This is the primitive Cell writer, user code is expected to use a higher level interface.
let tellCellObj (value:obj) : ClosedXMLOutput<unit> = 
    ClosedXMLOutput <| fun sheet pos ->  
        sheet.Cell(pos.RowIx, pos.ColIx).Value <- value
        (incrCol pos, ())

// This is the primitive Row writer, user code is expected to use a higher level interface.
let tellRowObjs (values:obj list) : ClosedXMLOutput<unit> =
    ClosedXMLOutput <| fun sheet pos ->
        ignore <| apply1 (mapMz tellCellObj values) sheet pos
        (nextRow pos, ())


// This will fail if it is not the first writer action.
let tellHeaders (values:string list) : ClosedXMLOutput<unit> =
    ClosedXMLOutput <| fun sheet pos ->
        if pos.RowIx = 1 && pos.ColIx = 1 then
            apply1 (tellCellObj <| List.map (fun s -> s :> obj) values) sheet pos
        else failwith "tellHeaders - not at first cell (something written already)"



// Experiment to make client code less stringy and more "typeful"....
// There seem to be two nice interfaces - an Applicative-like chain with (*>) or 
// List<CellWriter<unit>>.  
// In Haskell we would probably favour an Applicative chain, but using custom operators 
// seem a bit less pleasant in F#.
// Note though - the CellWriter/RowWriter model suits batch output it doesn't work for
// e.g. TotalOrder, TotalOrder2 where the ouput is split between procedures.


type CellWriter<'a> = private Wrapped of ClosedXMLOutput<'a>
type RowWriter<'a> = CellWriter<'a> list

let private getWrapped (cellWriter:CellWriter<'a>) : ClosedXMLOutput<'a> = 
    match cellWriter with | Wrapped(fn) -> fn


let tellRow (valueProcs:(CellWriter<unit>) list) : ClosedXMLOutput<unit> =
    ClosedXMLOutput <| fun sheet pos ->
        ignore <| apply1 (mapMz getWrapped valueProcs) sheet pos
        (nextRow pos, ())

let tellRows (records:seq<'a>) (writeRow:'a -> CellWriter<unit> list) : ClosedXMLOutput<unit> = 
    traverseMz (tellRow << writeRow) records

let tellRowsi (records:seq<'a>) (writeRow:int -> 'a -> CellWriter<unit> list) : ClosedXMLOutput<unit> = 
    traverseiMz (fun a ix -> tellRow <| writeRow a ix) records

let tellSheetWithHeaders (headers:string list) (records:seq<'a>) (writeRow:'a -> CellWriter<unit> list) : ClosedXMLOutput<unit> = 
    closedXMLOutput { do! tellHeaders headers
                      do! tellRows records writeRow }


let tellSheetWithHeadersi (headers:string list) (records:seq<'a>) (writeRow:int -> 'a -> CellWriter<unit> list) : ClosedXMLOutput<unit> = 
    closedXMLOutput { do! tellHeaders headers
                      do! tellRowsi records writeRow }

let tellObj (value:obj) : CellWriter<unit> = 
    Wrapped <| tellCellObj obj

let tellBool (value:bool) : CellWriter<unit> = tellObj (value :> obj)
let tellDateTime (value:System.DateTime) : CellWriter<unit> = tellObj (value :> obj)
let tellDecimal (value:decimal) : CellWriter<unit> = tellObj (value :> obj)
let tellFloat (value:float) : CellWriter<unit> = tellObj (value :> obj)
let tellGuid (value:System.Guid) : CellWriter<unit> = tellObj (value :> obj)   
let tellInteger (value:int) : CellWriter<unit> = tellObj (value :> obj)
let tellInteger64 (value:int64) : CellWriter<unit> = tellObj (value :> obj)

let tellString (value:string) : CellWriter<unit> = 
    match value with 
    | null -> tellObj ("" :> obj)
    | _ -> tellObj (value :> obj)
    
// tellStringf would be nice but not sure how to achieve it...

