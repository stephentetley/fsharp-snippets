module Scripts.NearestHospital

open FSharp.Data

open Microsoft.Office.Interop

open SL.Geo
open SL.ClosedXMLOutput

type HospitalsData = 
    CsvProvider< @"..\data\Accident-and-Emergency-Hospitals-Yorkshire.csv",
                 HasHeaders = true>

type HospitalsRow = HospitalsData.Row


[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type Hospital1 = 
    { HospitalName : string
      Address : string
      Phone : string
      Postcode : string
      LatLon : Coord.WGS84Point }

type HospitalList = Hospital1 list


let private buildHospitalList () = 
    let hospitalData = new HospitalsData()
    let make1 (rowi : HospitalsRow) : Hospital1 option = 
        match rowi.Name with 
        | null -> None
        | _ -> let optPt = Option.map Coord.osgb36GridToWGS84
                                <| Coord.tryReadOSGB36Grid rowi.``Grid Reference``
               match optPt with
               | Some pt -> Some <| { HospitalName = rowi.Name
                                    ; Address = rowi.Address 
                                    ; Postcode = rowi.Postcode
                                    ; Phone = rowi.Telephone
                                    ; LatLon = pt }
               | None -> None
    hospitalData.Rows |> Seq.map make1
                      |> Seq.toList
                      |> List.choose id

type BestMatch = { NearestHospital : Hospital1; DistanceToNearest: float<Coord.kilometer> }

let private tryClosestHosiptal (hospitals:HospitalList) (pt:Coord.WGS84Point) : BestMatch option =
    let find1 (dist,best) (hospital:Hospital1) = 
        let dist1 = Coord.haversineDistance pt hospital.LatLon
        if dist1 <= dist then
            (dist1, Some hospital)
        else (dist,best)
    List.fold find1 (50000.0<Coord.kilometer>, None) hospitals 
        |> fun (d,o) -> match o with 
                        | Some hosp -> Some { NearestHospital = hosp; DistanceToNearest = d }
                        | None -> None


type NearestHospitalAlgo<'asset> = 
    { TableHeaders      : option<string list> 
      ExtractLocation   : 'asset -> Coord.WGS84Point option
      OutputRow         : 'asset -> BestMatch option -> ClosedXMLOutput<unit> }
        

let generateNearestHospitalsXls (dict:NearestHospitalAlgo<'asset>) (source:'asset list) (outputFile:string) : unit =
    let hospitals = buildHospitalList ()
    
    let headerProc : ClosedXMLOutput<unit> = 
        match dict.TableHeaders with
        | None -> closedXMLOutput.Return ()
        | Some headers -> tellHeaders headers
    
    let rowProc (asset1:'asset) : ClosedXMLOutput<unit> = 
        let closest : BestMatch option = Option.bind (tryClosestHosiptal hospitals) (dict.ExtractLocation asset1) 
        dict.OutputRow asset1 closest
    
    let procOutput : ClosedXMLOutput<unit> = 
        closedXMLOutput { 
            do! headerProc
            do! mapMz rowProc source }

    outputToNew { SheetName = "Hospitals" } procOutput outputFile 
    ()