#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

//#r "Microsoft.Office.Interop.Excel"
//open Microsoft.Office.Interop

#load @"SL\AnswerMonad.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SQLUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.ScriptMonad

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



let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let rows = getHospitalImportRows ()
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| insertHospitals MakeDict rows 



// Note - input file has bad initial rows stopping type interence
type AssetDataset = 
    CsvProvider< @"G:\work\Projects\rtu\peer-to-peer.csv",
                 HasHeaders = true >

type AssetRow = AssetDataset.Row


let readAssetRows () : AssetRow list = 
    (new AssetDataset()).Rows |> Seq.toList


let nearestAlgo : NearestHospitalDict<AssetRow>  = 
    let extractLocation (row:AssetRow) : WGS84Point option = 
        Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Grid Reference``

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
        

