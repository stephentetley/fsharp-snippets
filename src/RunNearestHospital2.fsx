#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

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
#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper


#load @"Scripts\NearestHospital.fs"
open Scripts.NearestHospital


type ImportTable = 
    ExcelFile< @"G:\work\Projects\rtu\sites-NS-NGR.xlsx",
                SheetName = "NS_Sites",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }

let getImportRows () : ImportRow list = 
    excelTableGetRowsSeq importTableDict (new ImportTable()) |> Seq.toList




let nearestAlgo : NearestHospitalAlgo<ImportRow>  = 
    let extractLocation (row:ImportRow) : Coord.WGS84Point option = 
        Option.map Coord.osgb36GridToWGS84 <| Coord.tryReadOSGB36Grid row.NGR

    let outputRow (row:ImportRow) (obest : BestMatch option) : ClosedXMLOutput<unit> = 
        match obest with
        | None -> 
            tellRow [ tellString row.Uid
                    ; tellString row.Name
                    ; tellString row.NGR
                    ] 
        | Some bestMatch -> 
            let hospitalInfo : string = 
                sprintf "%s, %s, %s (Tel: %s)" 
                        bestMatch.NearestHospital.HospitalName
                        bestMatch.NearestHospital.Address
                        bestMatch.NearestHospital.Postcode
                        bestMatch.NearestHospital.Phone
            tellRow [ tellString row.Uid
                    ; tellString row.Name
                    ; tellString row.NGR
                    ; tellString hospitalInfo
                    ; tellFloat <| float bestMatch.DistanceToNearest  ] 

    { TableHeaders = Some <| [ "SAI"; "Name"; "NGR"; "Nearest Hospital"; "Distance" ]
    ; ExtractLocation = extractLocation
    ; OutputRow = outputRow
    } 

let main () = 
    let assetData = getImportRows ()
    generateNearestHospitalsXls nearestAlgo assetData @"G:\work\Projects\rtu\sites-with-hospital.xlsx"
        


