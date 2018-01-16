#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"

#load @"SL\CommonUtils.fs"
#load @"SL\AnswerMonad.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ClosedXMLOutput.fs"

#load @"SL\Coord.fs"
open SL.Geo

#load @"SL\ExcelUtils.fs"
open SL.ExcelUtils



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
        | _ -> let optPt = Option.map Coord.osgb36GridToWGS84
                            <| Coord.tryReadOSGB36Grid rowi.``Grid Ref``
               match optPt with
               | Some pt -> Some <| { Uid = rowi.``SAI Reference``
                                    ; Name = rowi.``Site Name``
                                    ; LatLon = pt }
               | None -> None
    inputData.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id

type BestMatch = float<Coord.kilometer> * Asset1

type OutputList = BestMatch list

let findClosest (pt : Coord.WGS84Point) (assets:AssetSet) : BestMatch option =
    let find1 (dist,best) (asset:Asset1) = 
        let dist1 = Coord.haversineDistance pt asset.LatLon
        if dist1 <= dist then
            (dist1, Some asset)
        else (dist,best)
    Set.fold find1 (50000.0<Coord.kilometer>, None) assets 
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
    List.rev <| step ancestor worklist [(0.0<Coord.kilometer>,ancestor)]

let main () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let allAssetData = Set.ofList <| buildAssetList ()
    
    let start = Option.get <| furthestNorth allAssetData
    let workingSet = Set.remove start <| allAssetData 

    let visitOrder = generateWorkList start workingSet
    List.iteri (fun i (dist,a:Asset1) -> printfn "%d,%f,%s,%s" (i+1) dist a.Uid a.Name) visitOrder

