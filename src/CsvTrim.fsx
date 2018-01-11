// Use FSharp.Data for CSV reading
#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

// Use Excel for csv-to-xls and xls-to-csv (defined in ExcelUtils)
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop
#load "ExcelUtils.fs"
open ExcelUtils

#load "CsvOutput.fs"
open CsvOutput

// NOTE - CSV processing with FSharp.Data is very fast
// To trim basic (macro-free) Xls files, going to and from CSV looks like
// a good choice, given that reading and writing Excel is very slow.


let inpath = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump.csv"
let outpath = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump-TRIM2.csv"



let truncRow (row:CsvRow) : CellWriter<unit> list = 
    let cols = Array.map (fun (x : string) -> x.Trim() |> testQuoteField |> tellString) row.Columns
    Array.toList cols

let trimCSV (inputFile:string) (outputFile:string) (csvHasHeaders:bool): unit =
    let csv = CsvFile.Load(uri=inputFile, hasHeaders=csvHasHeaders, quote='"')
    let proc = 
        traverseMz (tellRow << truncRow) csv.Rows
    outputToNew proc outputFile ","           


let main () = 
    trimCSV inpath outpath false



// Note - this test case has headers with commas that are double quoted.
// We have to treat the document as having no headers to render correctly.
let test01 () : unit = 
    let input = @"G:\work\Projects\T0975_EDM2\Kim.xlsx"
    let csv1 = @"G:\work\Projects\T0975_EDM2\Kim1.csv"
    let csv2 = @"G:\work\Projects\T0975_EDM2\Kim2.csv"
    let output = @"G:\work\Projects\T0975_EDM2\Kim-TRIM.xlsx"
    covertToCSV input csv1
    trimCSV csv1 csv2 false 
    covertToXlOpenXML csv2 output 

let test02 () = 
    printfn "%s" <| quoteField "\"Hello.\" said Peter."

// thinking about how to implement traverseM for a state monad...
let twolist : seq<string> = seq {
    yield "one"
    yield "two"
    yield! [] }

let test03 () = Seq.toList twolist

let seqMapAccumL (fn:'st -> 'a -> ('st * 'b)) (state:'st) (source:seq<'a>) : ('st * seq<'b>) = 
    let rec work (st:'st) (src:seq<'a>) = 
        if Seq.isEmpty src then (st, seq{ yield! [] })
        else 
            let a = Seq.head src
            let (st1,b) = fn st a
            let (st2,rest) = work st1 (Seq.tail src)
            (st2, seq { yield b; yield! rest })
    work state source

let test04 () = 
    let input = ["a"; "b"; "c"]
    seqMapAccumL (fun st a -> (st+1, String.replicate st a)) 1 (List.toSeq input)

