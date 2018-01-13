#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load @"SL\Geo.fs"
open SL.Geo

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput

type HospitalsTable = 
    ExcelFile< @"G:\work\Accident-and-Emergency-Hospitals-Yorkshire.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type HospitalsRow = HospitalsTable.Row

let test01 () = 
    let hosiptalData = new HospitalsTable()
    for (rowi:HospitalsRow) in hosiptalData.Data do
        match rowi.Name with
        | null -> printfn "<finished>"
        | _ -> let pt = Option.map Coord.osgb36GridToWGS84
                            <| Coord.tryReadOSGB36Grid rowi.``Grid Reference``
               printfn "%s, %s, %A" rowi.Name rowi.``Grid Reference`` pt

[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type Hospital1 = 
    { Name : string
      AddressString : string
      LatLon : Coord.WGS84Point }

type HospitalList = Hospital1 list


let buildHospitalList () = 
    let hospitalData = new HospitalsTable()
    let make1 (rowi : HospitalsRow) : Hospital1 option = 
        match rowi.Name with 
        | null -> None
        | _ -> let optPt = Option.map Coord.osgb36GridToWGS84
                                <| Coord.tryReadOSGB36Grid rowi.``Grid Reference``
               match optPt with
               | Some pt -> Some <| { Name = rowi.Name
                                    ; AddressString = sprintf "%s\n%s\n%s\n%s" rowi.Name rowi.Telephone rowi.Address rowi.Postcode
                                    ; LatLon = pt }
               | None -> None
    hospitalData.Data |> Seq.map make1
                      |> Seq.toList
                      |> List.choose id

type BestMatch = float<Coord.kilometer> * Hospital1

let tryClosestHosiptal (hospitals:HospitalList) (pt:Coord.WGS84Point) : BestMatch option =
    let find1 (dist,best) (hospital:Hospital1) = 
        let dist1 = Coord.haversineDistance pt hospital.LatLon
        if dist1 <= dist then
            (dist1, Some hospital)
        else (dist,best)
    List.fold find1 (50000.0<Coord.kilometer>, None) hospitals 
        |> fun (d,o) -> match o with 
                        | Some a -> Some (d,a)
                        | None -> None
        

type DerivedAssets = 
    ExcelFile< @"G:\work\Derived-Asset-List3.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type AssetRow = DerivedAssets.Row


let readAssetRows () : AssetRow list = 
    let assetData = new DerivedAssets()
    // In this case filter on column "B" (aka 1)
    // There are bad values in column "A"
    let nullPred (row:AssetRow) = match row.GetValue(1) with null -> false | _ -> true
    assetData.Data |> Seq.filter nullPred |> Seq.toList

let decodeBest (opt: BestMatch option) : string*string*float<Coord.kilometer>  = 
    match opt with
    | None -> ("Not found (ERROR)", "Not found (ERROR)", 0.0<Coord.kilometer>)
    | Some (dist,hospital) -> (hospital.Name, hospital.AddressString, dist)

let defaultIfNull (defaultValue:string) (check:string) = 
    match check with 
    | null -> defaultValue
    | _ -> check

let tellRow1 (hospitals:HospitalList) (row:AssetRow) : ClosedXMLOutput<unit> = 
    let opt = Option.map Coord.osgb36GridToWGS84
                            <| Coord.tryReadOSGB36Grid row.``Grid Ref``
    let best = Option.bind (tryClosestHosiptal hospitals) opt
    let (name,addr,dist) = decodeBest best 
    tellRow [ defaultIfNull "" <| row.UID
            ; defaultIfNull "" <| row.Name
            ; name
            ; addr
            ; sprintf "%f" (float dist)
            ]

let headers = [ "UID"; "Name"; "Hospital"; "Hospital Address"; "Distance" ]

let outputFile = @"G:\work\hospitals-output3.xlsx"

let main () = 
    let hosiptalData = buildHospitalList ()
    let assetData = readAssetRows ()
    let proc = 
        closedXMLOutput { do! tellHeaders headers
                          do! mapMz (tellRow1 hosiptalData) assetData }
    outputToNew proc outputFile "Hospitals"  

