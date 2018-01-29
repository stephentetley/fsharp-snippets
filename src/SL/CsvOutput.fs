module SL.CsvOutput

open System.IO

// NOTE - there is no real need for a corresponding CsvInput monad.
// For most Csv reading FSharp.Data type providers should be used, the
// main exception is trimming (unnecessarily) padded Csv.



// Quoting always uses double quote (it is not user customizable).
// I can't find much evidence that it should be user customizable.

type Separator = string

let quoteField (input:string) : string = 
    match input with
    | null -> "\"\""
    | _ -> sprintf "\"%s\"" (input.Replace("\"", "\"\""))

/// Quote a field containing comma
///
/// Warning- getting the arg order wrong of sep and input can lead to 
/// horrible to locate bugs. Possibly Separator should be wrapped but that seems an overhead too far.
let private testQuoteField (sep:Separator) (input:string)  : string = 
    match input with
    | null -> "\"\""
    | _ -> 
        if input.Contains(sep) || input.Contains("\n") then 
            quoteField input 
        else input


type CsvOutput<'a> = 
    CsvOutput of (StreamWriter -> Separator -> 'a)

let runCsvOutput (ma:CsvOutput<'a>) (handle:StreamWriter) (sep:Separator) : 'a =
    match ma with | CsvOutput f -> f handle sep



let inline private apply1 (ma : CsvOutput<'a>) (handle:StreamWriter) (sep:Separator) : 'a = 
    runCsvOutput ma handle sep

let private unitM (x:'a) : CsvOutput<'a> = 
    CsvOutput <| fun handle sep -> x

let private bindM (ma:CsvOutput<'a>) (f : 'a -> CsvOutput<'b>) : CsvOutput<'b> =
    CsvOutput <| fun handle sep -> 
        let a = apply1 ma handle sep in apply1 (f a) handle sep

let fail : CsvOutput<'a> = 
    CsvOutput (fun handle sep -> failwith "CsvOutput fail")

type CsvOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let csvOutput:CsvOutputBuilder = new CsvOutputBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:CsvOutput<'a>) : CsvOutput<'b> = 
    CsvOutput <| fun handle sep ->
        let a = apply1 ma handle sep in fn a


let mapM (fn: 'a -> CsvOutput<'b>) (xs: 'a list) : CsvOutput<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> CsvOutput<'b>) : CsvOutput<'b list> = mapM fn xs


let mapMz (fn: 'a -> CsvOutput<'b>) (xs: 'a list) : CsvOutput<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> CsvOutput<'b>) : CsvOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> CsvOutput<'b>) (source:seq<'a>) : CsvOutput<seq<'b>> = 
    CsvOutput <| fun handle sep ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle sep) source


let traverseiM (fn: int ->  'a -> CsvOutput<'b>) (source:seq<'a>) : CsvOutput<seq<'b>> = 
    CsvOutput <| fun handle sep ->
        Seq.mapi (fun ix x -> let mf = fn ix x in apply1 mf handle sep) source


// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> CsvOutput<'b>) (source:seq<'a>) : CsvOutput<unit> = 
    CsvOutput <| fun handle sep ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle sep in ac) 
                 () 
                 source 

let traverseiMz (fn: int -> 'a -> CsvOutput<'b>) (source:seq<'a>) : CsvOutput<unit> = 
    CsvOutput <| fun handle sep ->
        ignore <| Seq.fold (fun ix x -> 
                            let ans  = apply1 (fn ix x) handle sep in (ix+1))
                            0
                            source 

let mapiM (fn: 'a -> int -> CsvOutput<'b>) (xs: 'a list) : CsvOutput<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let mapiMz (fn: 'a -> int -> CsvOutput<'b>) (xs: 'a list) : CsvOutput<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

// CsvOutput-specific operations

type CsvOptions = 
    { Separator: string }

/// Should monadic function be first or second argument?
let outputToNew (options:CsvOptions) (ma:CsvOutput<'a>) (fileName:string) : 'a =
    use sw = new System.IO.StreamWriter(fileName)
    runCsvOutput ma sw options.Separator

let askSep : CsvOutput<Separator> = 
    CsvOutput <| fun _ sep -> sep

let tellRowStrings (values:string list) : CsvOutput<unit> =
    CsvOutput <| fun handle sep ->
        let line = String.concat sep values
        handle.WriteLine line

let tellHeaders (values:string list) : CsvOutput<unit> =
    bindM askSep 
          (fun sep -> tellRowStrings <| List.map (fun s -> testQuoteField sep s) values)


// Design note - previous CellWriter had a phantom type paramater
// but as CellWriter was never a monad it wasn't actually useful.

type CellWriter = private Wrapped of (Separator -> string)
type RowWriter = CellWriter list

let private getWrapped (sep:Separator) (cellWriter:CellWriter) : string = 
    match cellWriter with 
    | Wrapped(fn) -> match fn sep with | null -> "" | s -> s


let tellRow (rowWriter:RowWriter) : CsvOutput<unit> =
    bindM askSep (fun sep -> List.map (getWrapped sep) rowWriter |> tellRowStrings )

    
let tellRows (rowWriters:RowWriter list) : CsvOutput<unit> =
    mapMz tellRow rowWriters


let tellRecord (a:'a) (writeProc:'a -> RowWriter) : CsvOutput<unit> =
    bindM askSep (fun sep -> List.map (getWrapped sep) (writeProc a) |> tellRowStrings )


let tellRecords (records:seq<'a>) (writeProc:'a -> RowWriter) : CsvOutput<unit> = 
    traverseMz (tellRow << writeProc) records

let tellRecordsi (records:seq<'a>) (writeProc:int -> 'a -> RowWriter) : CsvOutput<unit> = 
    traverseiMz (fun a ix -> tellRow <| writeProc a ix) records

let writeRowsWithHeaders (headers:string list) (rows:seq<RowWriter>) : CsvOutput<unit> = 
    csvOutput { do! tellHeaders headers
                do! traverseMz tellRow rows }

let writeRecordsWithHeaders (headers:string list) (records:seq<'a>) (writeRow:'a -> RowWriter) : CsvOutput<unit> = 
    csvOutput { do! tellHeaders headers
                do! tellRecords records writeRow }


let writeRecordstWithHeadersi (headers:string list) (records:seq<'a>) (writeRow:int -> 'a -> RowWriter) : CsvOutput<unit> = 
    csvOutput { do! tellHeaders headers
                do! tellRecordsi records writeRow }


// Should testQuotedString be the default?
let tellObj (value:obj) : CellWriter = 
    Wrapped <| fun sep -> value.ToString() |> testQuoteField sep 

let tellBool (value:bool) : CellWriter = tellObj (value :> obj)
let tellDateTime (value:System.DateTime) : CellWriter = tellObj (value :> obj)
let tellDecimal (value:decimal) : CellWriter = tellObj (value :> obj)
let tellFloat (value:float) : CellWriter = tellObj (value :> obj)
let tellGuid (value:System.Guid) : CellWriter = tellObj (value :> obj)   
let tellInteger (value:int) : CellWriter = tellObj (value :> obj)
let tellInteger64 (value:int64) : CellWriter = tellObj (value :> obj)


let tellInt (value:int) : CellWriter = tellObj (value :> obj)
let tellInt64 (value:int64) : CellWriter = tellObj (value :> obj)


let tellString (value:string) : CellWriter = Wrapped <| fun sep -> testQuoteField sep value
let tellQuotedString (value:string) : CellWriter = Wrapped <| fun _ -> quoteField value

    