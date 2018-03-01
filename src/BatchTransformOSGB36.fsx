#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#load @"SL\Tolerance.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\CsvOutput.fs"
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.ExcelProviderHelper
open SL.CsvOutput

type InputTable = 
    ExcelFile< @"G:\work\Projects\rtu\Erskines\Erskine Site List.xlsx",
                SheetName = "Site List",
                ForceString = true >

type InputRow = InputTable.Row

let getImportRows () : seq<InputRow> = 
    let dict : GetRowsDict<InputTable, InputRow> = 
        { GetRows     = fun inputs -> inputs.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRowsSeq dict (new InputTable())

let headers = 
    [ "Index"; "Name"; "WKT" ]

let outputRow (ix:int) (row:InputRow) : RowWriter = 
    let trafoPoint = makeWktPoint wktIsoWGS84 << osgb36ToWGS84 
    [ tellInt (ix+1)
    ; tellQuotedString <| row.``Site Name``
    ; tellQuotedString << showWktPoint << trafoPoint <| readOSGB36Point row.NGR ]

let main () : unit = 
    let outputFile = @"G:\work\Projects\rtu\Erskines\erskines-for-qgis.csv"
    let rowWriters = Seq.mapi outputRow <| getImportRows ()
    let csvProc:CsvOutput<unit> = writeRowsWithHeaders headers rowWriters
    SL.CsvOutput.outputToNew {Separator=","} csvProc outputFile
