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

#load "CsvWriter.fs"
open CsvWriter

// NOTE - CSV processing with FSharp.Data is very fast
// To trim basic (macro-free) Xls files, going to and from CSV looks like
// a good choice, given that reading and writing Excel is very slow.


let inpath = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump.csv"
let outpath = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump-TRIM2.csv"



let truncRow (row:CsvRow) : string list = 
    let cols = Array.map (fun (x : string) -> testQuoteField <| x.Trim()) row.Columns
    Array.toList cols

let trimCSV (inputFile:string) (outputFile:string) (csvHasHeaders:bool): unit =
    let csv = CsvFile.Load(uri=inputFile, hasHeaders=csvHasHeaders, quote='"')
    let proc = 
        traverseMz (fun (row:CsvRow) -> tellRow (truncRow row)) csv.Rows
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