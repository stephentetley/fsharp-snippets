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
    ExcelFile< @"G:\work\Projects\rtu\y5-site-list.xlsx",
                SheetName = "Site List",
                ForceString = true >

type InputRow = InputTable.Row

let getImportRows () : InputRow list = 
    let dict : GetRowsDict<InputTable, InputRow> = 
        { GetRows     = fun inputs -> inputs.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new InputTable())

let headers = 
    [ "Index"; "Name"; "WKT"; "Work_Center" ]

let outputRow (ix:int) (row:InputRow) : RowWriter option = 
    let trafoPoint = makeWktPoint wktIsoWGS84 << osgb36ToWGS84
    let succK (osgbPt:OSGB36Point) : RowWriter =  
        [ tellInt (ix+1)
        ; tellQuotedString  <| row.``SAI Site Name``
        ; tellQuotedString << showWktPoint <| trafoPoint osgbPt
        ; tellQuotedString  <| row.``Operation Resp`` ]
        
    Option.map succK <| tryReadOSGB36Point row.NGR
    

let main () : unit = 
    let outputFile = @"G:\work\Projects\rtu\y5-for-qgis.csv"
    let rowWriters = List.choose id << List.mapi outputRow <| getImportRows ()
    let csvProc:CsvOutput<unit> = writeRowsWithHeaders headers rowWriters
    SL.CsvOutput.outputToNew {Separator=","} csvProc outputFile
