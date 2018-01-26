#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load @"SL\Coord.fs"
open SL.Geo

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput

#load @"Scripts\NearestHospital.fs"
open Scripts.NearestHospital


// Note - input file has bad initial rows stopping type interence
type AssetDataset = 
    CsvProvider< @"G:\work\Projects\rtu\peer-to-peer.csv",
                 HasHeaders = true >

type AssetRow = AssetDataset.Row


let readAssetRows () : AssetRow list = 
    (new AssetDataset()).Rows |> Seq.toList


let nearestAlgo : NearestHospitalDict<AssetRow>  = 
    let extractLocation (row:AssetRow) : Coord.WGS84Point option = 
        Option.map Coord.osgb36GridToWGS84 <| Coord.tryReadOSGB36Grid row.``Grid Reference``

    let outputRow (row:AssetRow) (obest : BestMatch option) : ClosedXMLOutput<unit> = 
        match obest with
        | None -> 
            tellRow [ tellString row.Reference
                    ; tellString row.``Common Name`` ] 
        | Some bestMatch -> 
            let hospitalLine = 
                sprintf "%s, %s, %s. Tel: %s" 
                        bestMatch.NearestHospital.HospitalName
                        bestMatch.NearestHospital.Address
                        bestMatch.NearestHospital.Postcode
                        bestMatch.NearestHospital.Phone
            tellRow [ tellString row.Reference
                    ; tellString row.``Common Name``
                    ; tellString bestMatch.NearestHospital.HospitalName
                    ; tellString hospitalLine
                    ; tellFloat <| float bestMatch.DistanceToNearest  ] 

    { TableHeaders = Some <| [ "SAI"; "Name"; "Hospital"; "Hospital Details"; "Distance" ]
    ; ExtractLocation = extractLocation
    ; OutputRow = outputRow
    } 

let main () = 
    let assetData = readAssetRows ()
    generateNearestHospitalsXls nearestAlgo assetData @"G:\work\Projects\rtu\p2p-sites-with-hospital.xlsx"
        

