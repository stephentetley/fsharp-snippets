#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\Npgsql.3.2.7\lib\net451"
#I @"..\packages\System.Threading.Tasks.Extensions.4.4.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.3\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

//#r "Microsoft.Office.Interop.Excel"
//open Microsoft.Office.Interop

#load @"SL\AnswerMonad.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SQLUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\PostGIS.fs"
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad


#load @"Scripts\NearestHospital.fs"
open Scripts.NearestHospital



let SetupDB(password:string) : unit = 
    let rows = getHospitalImportRows ()
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| insertHospitals MakeDict rows 



// Note - input file has bad initial rows stopping type interence (???)
type AssetDataset = 
    CsvProvider< @"G:\work\Projects\rtu\peer-to-peer.csv",
                 HasHeaders = true >

type AssetRow = AssetDataset.Row

let readAssetRows () : AssetRow list = (new AssetDataset()).Rows |> Seq.toList



let nearestMethodDict : NearestHospitalDict<AssetRow>  = 
    let extractLocation (row:AssetRow) : WGS84Point option = 
        Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.``Grid Reference``

    let outputRow (row:AssetRow) (optBest : BestMatch option) : SL.CsvOutput.RowWriter = 
        match optBest with
        | None -> 
            [ SL.CsvOutput.tellString row.Reference
            ; SL.CsvOutput.tellString row.``Common Name`` ] 
        | Some bestMatch -> 
            let hospitalLine = 
                sprintf "%s, %s, %s. Tel: %s" 
                        bestMatch.NearestHospital.HospitalName
                        bestMatch.NearestHospital.Address
                        bestMatch.NearestHospital.Postcode
                        bestMatch.NearestHospital.Phone
            [ tellQuotedString      row.Reference
            ; tellQuotedString      row.``Common Name``
            ; tellQuotedString      bestMatch.NearestHospital.HospitalName
            ; tellQuotedString      hospitalLine
            ; tellFloat             <| float bestMatch.Distance]

    { CsvHeaders = [ "Uid"; "Name"; "Hospital"; "Hospital Details"; "Distance" ]
    ; ExtractLocation = extractLocation
    ; OutputCsvRow = outputRow
    } 

let main (password:string) : unit = 
    let assetData = readAssetRows ()
    let outputFile = @"G:\work\Projects\rtu\p2p-sites-with-hospital2.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A") conn 
        <| generateNearestHospitalsCsv nearestMethodDict assetData outputFile
        

