// Use FSharp.Data for CSV processing

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

// Use Excel for csv-to-xls and xls-to-csv
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


#load "ExcelUtils.fs"
open ExcelUtils


// NOTE - CSV processing is very fast
// To trim basic (macro-free) Xls files, going to and from CSV looks like
// a good choice, given that Excel is very slow.

//let inpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.csv")
//let outpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.out.csv")

let inpath = @"G:\work\rtu\RTS\RTS-outstation-dump.csv"
let outpath = @"G:\work\rtu\RTS\RTS-outstation-dump-TRIM1.csv"


// Err... Headers don't seem to be double quoted if contain commas
// but subsequent rows are okay. This doesn't fix things... 
let safe (s:string) : string =
    if s.Contains(",") then sprintf "\"%s\"" s else s

let truncRow (parent:CsvFile) (rowi:CsvRow) : CsvRow = 
    let cols = Array.map (fun (x : string) -> x.Trim()) rowi.Columns
    new CsvRow(parent, cols)

let trimCSV (inputFile:string) (outputFile:string) (csvHasHeaders:bool): unit =
    let csv = CsvFile.Load(uri=inputFile, hasHeaders=csvHasHeaders, quote='"')
    let trimRow : CsvRow -> CsvRow = truncRow csv
    let out = csv.Map (System.Func<CsvRow,CsvRow>trimRow)
    out.Save(path = outputFile, separator=',')

let main () = 
    let rtsIn = CsvFile.Load(inpath)
    let tr = fun row -> truncRow rtsIn row
    let out = rtsIn.Map  (System.Func<CsvRow,CsvRow>tr)
    out.Save(path = outpath, separator=',')


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