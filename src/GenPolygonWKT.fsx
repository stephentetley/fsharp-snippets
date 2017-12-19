#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Coord.fs"
#load "GeoDistance.fs"
open Coord
open GeoDistance


// TODO - all this code is just a placeholder
// Postgis can build bounding polygons so we should interface 
// with that.

type InputTable = 
    ExcelFile< @"G:\work\rtu\IS_barriers\IS_Barriers.xlsx",
               SheetName = "RTU AR",
               ForceString = true >

type InputRow = InputTable.Row


type CoordDB = Map<string,Coord.WGS84Point>


let buildCoordDatabase () : CoordDB = 
    let inputData = new InputTable()
    let addLine (db:CoordDB) (rowi:InputRow) = 
        match rowi.``Site Name`` with
        | null -> db
        | _ ->  let opt = Option.map Coord.enToLatLon  <| Coord.fromOSGridRef10 rowi.``Grid Ref``
                match opt with
                | Some(pt:Coord.WGS84Point) -> Map.add rowi.``Site Name`` pt db
                | None ->db
    inputData.Data 
        |> Seq.fold addLine Map.empty
 

let genWKT (order:string list) (db:CoordDB) : string =
    let make1 (pt:Coord.WGS84Point) : string = sprintf "%f %f" pt.Longitude pt.Latitude
    let coords =
        List.fold (fun ac name -> 
            match Map.tryFind name db with
            | Some(pt) -> (pt :: ac)
            | None -> ac)
            []
            order
    let body = String.concat "," <| List.map make1 coords
    sprintf "oid;wkt\n1;\"LINESTRING(%s)\"" body

let outpath = @"G:\work\rtu\IS_barriers\polygons.csv"

 

let main () = 
    let db = buildCoordDatabase ()
    let siteOrder : string list = 
        System.IO.File.ReadLines(inputList)
            |> Seq.filter (fun (s:string) -> not <| System.String.IsNullOrEmpty s)
            |> Seq.toList
    let output = genWKT siteOrder db
    System.IO.File.WriteAllText (outpath, output)
    printfn "%s" output

