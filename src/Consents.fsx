// Use ExcelProvider...
#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\Coord.fs"
open SL.Geo

#load @"SL\CsvOutput.fs"
open SL.CsvOutput

#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper


type ConsentsTable = 
    ExcelFile< @"G:\work\Projects\events2\Consents-eastings-northings.xlsx",
               SheetName = "Sheet1",
               ForceString = false >

type ConsentsRow = ConsentsTable.Row

let consentsTableDict : GetRowsDict<ConsentsTable, ConsentsRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.``Common Name`` with null -> false | _ -> true }

let getConsentsRows () : ConsentsRow list = excelTableGetRows consentsTableDict (new ConsentsTable())

let tellConsentsRow (row:ConsentsRow) : CsvOutput<unit> = 
    match row.``Common Name`` with
    | null -> csvOutput.Return ()
    | _ -> let point : Coord.OSGB36Point = 
                let east = row.``Outfall NGRE`` * 1.0<meter>
                let north = row.``Outfall NGRN`` * 1.0<meter>
                { Coord.Easting = east; Coord.Northing = north }
           tellRow [ tellString row.``AIB Reference``
                   ; tellString row.``Common Name`` 
                   ; tellString (Coord.showOSGB36Point point) ]



let main () : unit = 
    let outfile = @"G:\work\Projects\events2\Consents-Gridref.csv"
    let rows:ConsentsRow list= getConsentsRows ()
    let procM : CsvOutput<unit> = 
        csvOutput { 
            do! tellHeaders ["UID"; "Name" ; "Grid Ref"]
            do! mapMz tellConsentsRow rows }
                    
    outputToNew {Separator=","} procM outfile
    
