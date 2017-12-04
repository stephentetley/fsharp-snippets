#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Coord.fs"
open Coord

#load "ExcelUtils.fs"
open ExcelUtils

#load "GeoDistance.fs"
open GeoDistance


type InputTable = 
    ExcelFile< @"G:\work\Projects\pgrouting\IS Barriers RTU & Enhancements.xlsx",
               SheetName = "RTU AR",
               ForceString = true >

type InputRow = InputTable.Row




[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type Asset1 = 
    { Uid : string
      Name : string
      LatLon : Coord.WGS84Point }

type AssetList = Asset1 list
type AssetSet = Set<Asset1>

let buildAssetList () = 
    let inputData = new InputTable()
    let make1 (rowi : InputRow) : Asset1 option = 
        match rowi.``Site Name`` with 
        | null -> None
        | _ -> let optPt = Option.map Coord.enToLatLon  <| Coord.fromOSGridRef10 rowi.``Grid Ref``
               match optPt with
               | Some pt -> Some <| { Uid = rowi.``SAI Reference``
                                    ; Name = rowi.``Site Name``
                                    ; LatLon = pt }
               | None -> None
    inputData.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id

type BestMatch = float<GeoDistance.kilometer> * Asset1

type OutputList = BestMatch list

let findClosest (pt : Coord.WGS84Point) (assets:AssetSet) : BestMatch option =
    let find1 (dist,best) (asset:Asset1) = 
        let dist1 = GeoDistance.haversineDistance pt asset.LatLon
        if dist1 <= dist then
            (dist1, Some asset)
        else (dist,best)
    Set.fold find1 (50000.0<GeoDistance.kilometer>, None) assets 
        |> fun (d,o) -> match o with 
                        | Some a -> Some (d,a)
                        | None -> None

let findAsset (cmp : Asset1 -> Asset1 -> bool) (nodes:AssetSet) : Asset1 option = 
    let find1 (ac: Asset1 option) (elem:Asset1) = 
        match ac with
        | None -> Some elem
        | Some ac1 -> 
            if cmp elem ac1 then
                Some elem
            else ac
    Set.fold find1 None nodes
    
let furthestNorth (nodes:AssetSet) : Asset1 option = 
    findAsset (fun elem ac ->  elem.LatLon.Latitude > ac.LatLon.Latitude) nodes

let furthestSouth (nodes:AssetSet) : Asset1 option = 
    findAsset (fun elem ac ->  elem.LatLon.Latitude < ac.LatLon.Latitude) nodes

let cellIndex (col:string) (row:int) : string = 
    sprintf "%s%d" col row





let findStep (ancestor:Asset1) (worklist:AssetSet) : (BestMatch * AssetSet) option = 
    let makeResult ans = (ans, Set.remove (snd ans) worklist)
    Option.map makeResult <| findClosest ancestor.LatLon worklist

let generateWorkList (ancestor:Asset1) (worklist:AssetSet) : OutputList = 
    let rec step a1 assets ac = 
        match findStep a1 assets with
        | None -> ac
        | Some(ans,rest) -> step (snd ans) rest (ans :: ac) 
    List.rev <| step ancestor worklist [(0.0<GeoDistance.kilometer>,ancestor)]

let main () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let allAssetData = Set.ofList <| buildAssetList ()
    
    let start = Option.get <| furthestNorth allAssetData
    let workingSet = Set.remove start <| allAssetData 

    let visitOrder = generateWorkList start workingSet
    List.iteri (fun i (dist,a:Asset1) -> printfn "%d,%f,%s,%s" (i+1) dist a.Uid a.Name) visitOrder

