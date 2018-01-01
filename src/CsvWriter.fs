module CsvWriter

open System.IO

// Quoting always uses double quote (it is not user customizable).
// I can't find much evidence that it should be user customizable.

let quoteField (input:string) : string = 
    match input with
    | null -> "\"\""
    | _ -> sprintf "\"%s\"" (input.Replace("\"", "\"\""))

// Quote a field containing comma
let testQuoteField (input:string) : string = 
    match input with
        | null -> "\"\""
        | _ -> if input.Contains(",") then quoteField input else input



type Separator = string

type CsvWriter<'a> = 
    CsvWriter of (StreamWriter -> Separator -> 'a)

let runCsvWriter (ma:CsvWriter<'a>) (handle:StreamWriter) (sep:Separator) : 'a =
    match ma with
    | CsvWriter(f) -> f handle sep



let inline private apply1 (ma : CsvWriter<'a>) (handle:StreamWriter) (sep:Separator) : 'a = 
    runCsvWriter ma handle sep

let private unitM (x:'a) : CsvWriter<'a> = 
    CsvWriter <| fun handle sep -> x

let private bindM (ma:CsvWriter<'a>) (f : 'a -> CsvWriter<'b>) : CsvWriter<'b> =
    CsvWriter <| fun handle sep -> 
        let a = apply1 ma handle sep in apply1 (f a) handle sep

let fail : CsvWriter<'a> = 
    CsvWriter (fun handle sep -> failwith "CsvWriter fail")

type CsvWriterBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let csvWriter:CsvWriterBuilder = new CsvWriterBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:CsvWriter<'a>) : CsvWriter<'b> = 
    CsvWriter <| fun handle sep ->
        let a = apply1 ma handle sep in fn a


let mapM (fn: 'a -> CsvWriter<'b>) (xs: 'a list) : CsvWriter<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> CsvWriter<'b>) : CsvWriter<'b list> = mapM fn xs


let mapMz (fn: 'a -> CsvWriter<'b>) (xs: 'a list) : CsvWriter<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> CsvWriter<'b>) : CsvWriter<unit> = mapMz fn xs

let traverseM (fn: 'a -> CsvWriter<'b>) (source:seq<'a>) : CsvWriter<seq<'b>> = 
    CsvWriter <| fun handle sep ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle sep) source


let traverseiM (fn: int ->  'a -> CsvWriter<'b>) (source:seq<'a>) : CsvWriter<seq<'b>> = 
    CsvWriter <| fun handle sep ->
        Seq.mapi (fun ix x -> let mf = fn ix x in apply1 mf handle sep) source


// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> CsvWriter<'b>) (source:seq<'a>) : CsvWriter<unit> = 
    CsvWriter <| fun handle sep ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle sep in ac) 
                 () 
                 source 

let traverseiMz (fn: int -> 'a -> CsvWriter<'b>) (source:seq<'a>) : CsvWriter<unit> = 
    CsvWriter <| fun handle sep ->
        ignore <| Seq.fold (fun ix x -> 
                            let ans  = apply1 (fn ix x) handle sep in (ix+1))
                            0
                            source 

let mapiM (fn: 'a -> int -> CsvWriter<'b>) (xs: 'a list) : CsvWriter<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let mapiMz (fn: 'a -> int -> CsvWriter<'b>) (xs: 'a list) : CsvWriter<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

// CsvWriter-specific operations

let outputToNew (ma:CsvWriter<'a>) (fileName:string) (sep:Separator) : 'a =
    use sw = new System.IO.StreamWriter(fileName)
    runCsvWriter ma sw sep


let tellRowStrings (values:string list) : CsvWriter<unit> =
    CsvWriter <| fun handle sep ->
        let line = String.concat sep values
        handle.WriteLine line

let tellHeaders (values:string list) : CsvWriter<unit> =
    tellRowStrings <| List.map testQuoteField values

// Make client code less stringy and more "typeful"....

type CellWriter<'a> = private Wrapped of string
type RowWriter<'a> = CellWriter<'a> list

let private getWrapped (cellWriter:CellWriter<'a>) : string = 
    match cellWriter with | Wrapped(s) -> match s with | null -> "" | _ -> s

let tellRow (valueProcs:(CellWriter<unit>) list) : CsvWriter<unit> =
    tellRowStrings <| List.map getWrapped valueProcs


let tellRows (records:seq<'a>) (writeRow:'a -> CellWriter<unit> list) : CsvWriter<unit> = 
    traverseMz (tellRow << writeRow) records

let tellRowsi (records:seq<'a>) (writeRow:int -> 'a -> CellWriter<unit> list) : CsvWriter<unit> = 
    traverseiMz (fun a ix -> tellRow <| writeRow a ix) records

let tellSheetWithHeaders (headers:string list) (records:seq<'a>) (writeRow:'a -> CellWriter<unit> list) : CsvWriter<unit> = 
    csvWriter { do! tellHeaders headers
                do! tellRows records writeRow }


let tellSheetWithHeadersi (headers:string list) (records:seq<'a>) (writeRow:int -> 'a -> CellWriter<unit> list) : CsvWriter<unit> = 
    csvWriter { do! tellHeaders headers
                do! tellRowsi records writeRow }

let tellObj (value:obj) : CellWriter<unit> = Wrapped <| value.ToString()

let tellBool (value:bool) : CellWriter<unit> = tellObj (value :> obj)
let tellDateTime (value:System.DateTime) : CellWriter<unit> = tellObj (value :> obj)
let tellDecimal (value:decimal) : CellWriter<unit> = tellObj (value :> obj)
let tellFloat (value:float) : CellWriter<unit> = tellObj (value :> obj)
let tellGuid (value:System.Guid) : CellWriter<unit> = tellObj (value :> obj)   
let tellInteger (value:int) : CellWriter<unit> = tellObj (value :> obj)
let tellInteger64 (value:int64) : CellWriter<unit> = tellObj (value :> obj)

let tellString (value:string) : CellWriter<unit> = Wrapped <| value
let tellQuotedString (value:string) : CellWriter<unit> = tellString <| quoteField value