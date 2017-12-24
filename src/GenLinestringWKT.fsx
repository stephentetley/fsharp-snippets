#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Coord.fs"
open Coord


type InputTable = 
    ExcelFile< @"G:\work\Projects\rtu\IS_barriers\IS_Barriers.xlsx",
               SheetName = "RTU AR",
               ForceString = true >

type InputRow = InputTable.Row


type CoordDB = Map<string,Coord.WGS84Point>


let buildCoordDatabase () : CoordDB = 
    let inputData = new InputTable()
    let addLine (db:CoordDB) (rowi:InputRow) = 
        match rowi.``Site Name`` with
        | null -> db
        | _ ->  let opt = Option.map (Coord.enToLatLon << Coord.osgb36GridToPoint)
                            <| Coord.tryReadOSGB36Grid  rowi.``Grid Ref``
                match opt with
                | Some(pt:Coord.WGS84Point) -> Map.add rowi.``Site Name`` pt db
                | None ->db
    inputData.Data 
        |> Seq.fold addLine Map.empty
 

type OrderGroups = (string list) list

let genLINESTRING (coords:Coord.WGS84Point list) : string =
    let make1 (pt:Coord.WGS84Point) : string = sprintf "%f %f" pt.Longitude pt.Latitude
    let body = String.concat "," <| List.map make1 coords
    sprintf "\"LINESTRING(%s)\"" body

let findPoints (sites:string list)  (db:CoordDB) : Coord.WGS84Point list = 
    List.fold (fun ac name -> 
        match Map.tryFind name db with
        | Some(pt) -> (pt :: ac)
        | None -> ac)
        []
        sites

let genWKT (orders:OrderGroups) (db:CoordDB) : string =
    let pointGroups = List.map (fun ss -> findPoints ss db) orders
    let textlines = 
        List.mapi (fun i pts -> sprintf "%i;%s" (i+1) (genLINESTRING pts)) pointGroups
    String.concat "\n" ("oid;wkt" :: textlines)

let partition (lines:string list) : OrderGroups = 
    let safecons (xs:string list) (xss:OrderGroups) = 
        match xs with 
        | [] -> xss
        | _ -> (xs::xss)
    let rec go (rest:string list) (ac1:string list) (acAll:OrderGroups) =
        match rest with
        | [] -> List.rev (safecons ac1 acAll)
        | x :: xs ->
            if System.String.IsNullOrEmpty x then
                go xs [] (safecons (List.rev ac1) acAll)
            else go xs (x::ac1) acAll
    go lines [] []

let outpath = @"G:\work\rtu\IS_barriers\siteorder-output.csv"

let inputList = @"G:\work\rtu\IS_barriers\sites-in-order.txt"
 

let main () = 
    let db = buildCoordDatabase ()
    let siteOrders:OrderGroups = 
        System.IO.File.ReadLines(inputList)
            |> Seq.toList 
            |> partition
    let output = genWKT siteOrders db
    System.IO.File.WriteAllText (outpath, output)
    printfn "%s" output
