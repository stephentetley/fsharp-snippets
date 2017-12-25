// Use ExcelProvider...
#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames


#load "Geo.fs"
open Geo

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

let printRow (sw:System.IO.StreamWriter) (row:ConsentsRow) : unit = 
    match row.``Common Name`` with
    | null -> ()
    | _ -> let pt : Coord.OSGB36Point = 
                let E = row.``Outfall NGRE`` * 1.0<meter>
                let N = row.``Outfall NGRN`` * 1.0<meter>
                { Coord.Eastings = E;
                  Coord.Northings = N }
           let gridref = Coord.osgb36PointToGrid pt
           fprintf sw "%s,\"%s\",%s\n" 
                        row.``AIB Reference``
                        row.``Common Name`` 
                        (Coord.showOSGB36Grid gridref)



let test02 () : unit = 
    let file = new ConsentsTable()
    use sw = new System.IO.StreamWriter(@"G:\work\Projects\T0975_EDM2\Consents-Gridref.csv")
    for (rowi:ConsentsRow) in file.Data do
        match rowi.``Common Name`` with
        | null -> ()
        | _ -> printRow sw rowi
    sw.Close ()

