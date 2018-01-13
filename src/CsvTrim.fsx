// Use FSharp.Data for CSV reading
#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

// Use Excel for csv-to-xls and xls-to-csv (defined in ExcelUtils)
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load @"SL\CommonUtils.fs"
open SL.CommonUtils

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\ClosedXMLOutput.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ExcelUtils.fs"
open SL.ExcelUtils


open SL.CsvOutput

// NOTE - CSV processing with FSharp.Data is very fast
// To trim basic (macro-free) Xls files, going to and from CSV looks like
// a good choice, given that reading and writing Excel is very slow.


let csvPathIn = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump.csv"
let csvPathOut = @"G:\work\Projects\rtu\RTS\RTS-outstation-dump-TRIM2.csv"
let xlsPathIn = @"G:\work\Projects\events2\rts-outstations-jan2018.xlsx"
let xlsPathOutCsv = @"G:\work\Projects\events2\rts-outstations-jan2018-TRIM.csv"     
let xlsPathOutXls = @"G:\work\Projects\events2\rts-outstations-jan2018-TRIM.xlsx" 

let main () = 
    trimCsvFile csvPathIn csvPathOut false ","
    trimXlsFileToCsv xlsPathIn xlsPathOutCsv

let tooLong () = 
    trimXlsFile xlsPathIn xlsPathOutXls

// Note - this test case has headers with commas that are double quoted.
// We have to treat the document as having no headers to render correctly.
let test01 () : unit = 
    let input = @"G:\work\Projects\T0975_EDM2\Kim.xlsx"
    let csv1 = @"G:\work\Projects\T0975_EDM2\Kim1.csv"
    let csv2 = @"G:\work\Projects\T0975_EDM2\Kim2.csv"
    let output = @"G:\work\Projects\T0975_EDM2\Kim-TRIM.xlsx"
    covertToCSV input csv1
    trimCsvFile csv1 csv2 false ","
    covertToXlOpenXML csv2 output 



let test05 () = 
    suffixFileName @"G:\work\Projects\T0975_EDM2\Kim.xlsx" "-TRIM"

let  testQuoteField (input:string) (sep:Separator) : string = 
    match input with
    | null -> "\"\""
    | _ -> if input.Contains(sep) then quoteField input else input
