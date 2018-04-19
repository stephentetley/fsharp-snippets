// Use FSharp.Data for CSV reading
#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

// Use Excel for csv-to-xls and xls-to-csv (defined in ExcelUtils)
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\DocumentFormat.OpenXml.2.8.1\lib\net46\"
#I @"..\packages\FastMember.Signed.1.3.0\lib\net45\"
#I @"..\packages\ClosedXML.0.92.1\lib\net46\"
#r "ClosedXML"



#load @"SL\CommonUtils.fs"
#load @"SL\AnswerMonad.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\ClosedXMLOutput.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\CsvUtils.fs"
#load @"SL\ExcelUtils.fs"
open SL.CommonUtils
open SL.CsvOutput
open SL.CsvUtils
open SL.ExcelUtils

// NOTE - CSV processing with FSharp.Data is very fast
// To trim basic (macro-free) Xls files, going to and from CSV looks like
// a good choice, given that reading and writing Excel is very slow.


let csvPathIn       = @"G:\work\Projects\rtu\RTS\stations-dump.csv"
let csvPathOut      = @"G:\work\Projects\rtu\RTS\stations-dump-TRIM2.csv"
let xlsPathIn       = @"G:\work\Projects\events2\stations-jan2018.xlsx"
let xlsPathOutCsv   = @"G:\work\Projects\events2\stations-jan2018-TRIM.csv"     
let xlsPathOutXls   = @"G:\work\Projects\events2\stations-jan2018-TRIM.xlsx" 

let temp01 () = 
    let options = 
        { InputSeparator = ","
          InputHasHeaders = false
          OutputSeparator = "," }
    trimCsvFile options csvPathIn csvPathOut
    trimXlsFileToCsv xlsPathIn xlsPathOutCsv
    trimXlsSheet xlsPathIn xlsPathOutXls



// Note - this test case has headers with commas that are double quoted.
// We have to treat the document as having no headers to render correctly.
let test01 () : unit = 
    let input = @"G:\work\Projects\events2\stations.xlsx"
    let csv1 = @"G:\work\Projects\events2\stations1.csv"
    let csv2 = @"G:\work\Projects\events2\stations2.csv"
    let output = @"G:\work\Projects\events2\stations-TRIM.xlsx"
    let options = 
        { InputSeparator = ","
          InputHasHeaders = false
          OutputSeparator = "," }
    covertToCSV input csv1
    trimCsvFile options csv1 csv2
    covertToXlOpenXML csv2 output 



let test02 () = 
    suffixFileName @"G:\work\Projects\events2\stations.xlsx" "-TRIM"


let rtuTrim () = 
    let input  = @"G:\work\Projects\rtu\RTS-outstations-report.tab.csv"
    let output = @"G:\work\Projects\rtu\RTS-outstations-report.trim.csv"
    let options = 
        { InputSeparator = "\t"
          InputHasHeaders = false
          OutputSeparator = "," }
    trimCsvFile options input output