#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Geo.fs"
open Geo
#load "CsvOutput.fs"
open CsvOutput


type InputTable = 
    ExcelFile< @"G:\work\Projects\rtu\IS_barriers\IS_Barriers.xlsx",
               SheetName = "RTU AR",
               ForceString = true >

type InputRow = InputTable.Row


type CoordDB = Map<string, Coord.WGS84Point>


let buildCoordDatabase () : CoordDB = 
    let inputData = new InputTable()
    let addLine (db:CoordDB) (rowi:InputRow) = 
        match rowi.``Site Name`` with
        | null -> db
        | _ ->  let opt = Option.map Coord.osgb36GridToWGS84
                            <| Coord.tryReadOSGB36Grid  rowi.``Grid Ref``
                match opt with
                | Some(pt:Coord.WGS84Point) -> Map.add rowi.``Site Name`` pt db
                | None -> db
    inputData.Data 
        |> Seq.fold addLine Map.empty
 

type OrderGroups = (string list) list


let findPoints (sites:string list)  (db:CoordDB) : Geo.Coord.WGS84Point list = 
    List.fold (fun ac name -> 
        match Map.tryFind name db with
        | Some(pt) -> (pt :: ac)
        | None -> ac)
        []
        sites

let genWKT (orders:OrderGroups) (db:CoordDB) : CsvOutput<unit> =
    let pointGroups = List.map (fun ss -> findPoints ss db) orders
    tellSheetWithHeadersi   ["oid"; "wkt"] 
                            pointGroups
                            (fun ix pts -> 
                                [ tellInteger <| ix+1
                                ; tellQuotedString <| Wkt.genLINESTRING pts ])


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

let outpath = @"G:\work\Projects\rtu\IS_barriers\siteorder-output.csv"

let inputList = @"G:\work\Projects\rtu\IS_barriers\sites-in-order.txt"
 

let main () = 
    let db = buildCoordDatabase ()
    let siteOrders:OrderGroups = 
        System.IO.File.ReadLines(inputList)
            |> Seq.toList 
            |> partition
    outputToNew (genWKT siteOrders db) outpath ";"

