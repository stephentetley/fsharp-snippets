#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Geo.fs"
open Geo


type InputTable = 
    ExcelFile< @"G:\work\Projects\rtu\IS_barriers\IS_Barriers.xlsx",
               SheetName = "RTU AR",
               ForceString = true >

type InputRow = InputTable.Row



type CoordDB = Map<string,Coord.WGS84Point>


let main () = 
    let inputData = new InputTable()
    for (rowi:InputRow) in inputData.Data do
        match rowi.``Site Name`` with
        | null -> printfn "<finished>"
        | _ ->  let opt = Option.map Coord.osgb36GridToWGS84
                            <| Coord.tryReadOSGB36Grid rowi.``Grid Ref``
                match opt with
                | Some(pt:Coord.WGS84Point) ->  printfn "%s,%s,%s,%f,%f" rowi.``Site Name`` rowi.``Grid Ref`` rowi.``Operational Responsibility`` pt.Latitude pt.Longitude
                | None -> printfn "%s,%s,%s,0.0,0.0" rowi.``Site Name`` rowi.``Grid Ref`` rowi.``Operational Responsibility``
