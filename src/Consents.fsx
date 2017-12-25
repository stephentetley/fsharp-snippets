// Use ExcelProvider...
#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames


#load "Geo.fs"
open Geo

#load "CsvWriter.fs"
open CsvWriter


// ToDO should use CsvWriter

type ConsentsTable = 
    ExcelFile< @"G:\work\Projects\events2\Consents-eastings-northings.xlsx",
               SheetName = "Sheet1",
               ForceString = false >

type ConsentsRow = ConsentsTable.Row

let test01 () = 
    let file = new ConsentsTable()
    for (rowi:ConsentsRow) in file.Data do
        match rowi.``Common Name`` with
        | null -> printfn "<nullrow>"
        | _ ->
            printfn "%s, %f, %f" 
                    rowi.``Common Name`` 
                    rowi.``Outfall NGRE`` 
                    rowi.``Outfall NGRN``

let tellConsentsRow (row:ConsentsRow) : CsvWriter<unit> = 
    match row.``Common Name`` with
    | null -> csvWriter.Return ()
    | _ -> let pt : Coord.OSGB36Point = 
                let E = row.``Outfall NGRE`` * 1.0<meter>
                let N = row.``Outfall NGRN`` * 1.0<meter>
                { Coord.Eastings = E;
                  Coord.Northings = N }
           let gridref = Coord.osgb36PointToGrid pt
           tellRow [ row.``AIB Reference``
                   ; row.``Common Name`` 
                   ; (Coord.showOSGB36Grid gridref) ]



let main () : unit = 
    let input = new ConsentsTable()
    let outfile = @"G:\work\Projects\events2\Consents-Gridref.csv"
    let test (row:ConsentsRow) : bool = 
        match row.``Common Name`` with
        | null -> false
        | _ -> true
    let rows:seq<ConsentsRow> = input.Data |> Seq.filter test
    let procM : CsvWriter<unit> = 
        csvWriter { 
            do! tellRow ["UID"; "Name" ; "Grid Ref"]
            do! traverseMz tellConsentsRow rows }
                    
    outputToNew procM outfile ","
    
