#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data


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

#load @"Scripts\NearestHospital.fs"
open Scripts.NearestHospital


// Note - input file has bad initial rows stopping type interence
type AssetDataset = 
    CsvProvider< @"G:\work\Projects\events2\site-list-for-hospitals.csv",
                 Schema = "asset_sai_number=string, common_name=string, asset_type=string, work_category=string, toplevel_permit_ref=string, site_ngr=string, asset_status=string, site_address=string, site_postcode=string, operational_contact=string",
                 HasHeaders = true >

type AssetRow = AssetDataset.Row


let readAssetRows () : AssetRow list = 
    let assetData = new AssetDataset()
    assetData.Rows |> Seq.toList



let nearestAlgo : NearestHospitalAlgo<AssetRow>  = 
    let extractLocation (row:AssetRow) : Coord.WGS84Point option = 
        Option.map Coord.osgb36GridToWGS84 <| Coord.tryReadOSGB36Grid row.Site_ngr

    let outputRow (row:AssetRow) (obest : BestMatch option) : ClosedXMLOutput<unit> = 
        match obest with
        | None -> 
            tellRow [ tellString row.Asset_sai_number
                    ; tellString row.Common_name ] 
        | Some bestMatch -> 
            tellRow [ tellString row.Asset_sai_number
                    ; tellString row.Common_name
                    ; tellString bestMatch.NearestHospital.HospitalName
                    ; tellString bestMatch.NearestHospital.Address
                    ; tellFloat <| float bestMatch.DistanceToNearest  ] 

    { TableHeaders = Some <| [ "SAI"; "Name"; "Hospital"; "Hospital Address"; "Distance" ]
    ; ExtractLocation = extractLocation
    ; OutputRow = outputRow
    } 

let main () = 
    let assetData = readAssetRows ()
    generateNearestHospitalsXls nearestAlgo assetData @"G:\work\Projects\events2\sites-with-hospital.xlsx"
        

