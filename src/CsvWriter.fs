module CsvWriter

open System.IO

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

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> CsvWriter<'b>) (source:seq<'a>) : CsvWriter<unit> = 
    CsvWriter <| fun handle sep ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle sep in ac) 
                 () 
                 source 



// CsvWriter-specific operations

let outputToNew (ma:CsvWriter<'a>) (fileName:string) (sep:Separator) : 'a =
    use sw = new System.IO.StreamWriter(fileName)
    runCsvWriter ma sw sep


let tellRow (values:string list) : CsvWriter<unit> =
    CsvWriter <| fun handle sep ->
        let line = String.concat sep values
        handle.WriteLine line




