module Scripts.NearestHospital

open FSharp.Data
open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.SqlUtils
open SL.PGSQLConn
open SL.ScriptMonad
open SL.ClosedXMLOutput

// TODO - move towards using PostGIS

type HospitalsData = 
    CsvProvider< @"..\data\Accident-and-Emergency-Hospitals-Yorkshire.csv",
                 HasHeaders = true>

type HospitalsRow = HospitalsData.Row


let getHospitalImportRows () : seq<HospitalsRow> = 
    (new HospitalsData ()).Rows |> Seq.cast<HospitalsRow>


// ********** SCRIPT **********
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)


let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRowsRestartIdentity "spt_hospitals"



[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type HospitalRecord = 
    { HospitalName : string
      Address : string
      Phone : string
      Postcode : string
      LatLon : WGS84Point }

type HospitalInsertDict<'inputrow> = 
    { tryMakeHospitalRecord : 'inputrow -> HospitalRecord option }

let tryMakeRecord (row:HospitalsRow) : HospitalRecord option = 
    match tryReadOSGB36Point row.``Grid Reference`` with
    | Some osgb36 -> 
        Some <| { HospitalName = row.Name
                ; Address = row.Address
                ; Phone = row.Telephone
                ; Postcode = row.Postcode
                ; LatLon = osgb36ToWGS84 osgb36 }
    | _ -> None

let MakeDict : HospitalInsertDict<HospitalsRow> = { tryMakeHospitalRecord = tryMakeRecord }


let private makeHospitalINSERT (hospital:HospitalRecord) : string = 
    let makePointLit (pt:WGS84Point) : string = 
        sprintf "ST_GeogFromText('SRID=4326;%s')" (showWktPoint <| wgs84PointToWKT pt)
    sqlINSERT "spt_hospitals" 
            <|  [ stringValue       "name"              hospital.HospitalName
                ; stringValue       "telephone"         hospital.Phone
                ; stringValue       "address"           hospital.Address
                ; stringValue       "postcode"          hospital.Postcode
                ; literalValue      "grid_ref"          <| makePointLit hospital.LatLon
                ]


let insertHospitals (dict:HospitalInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    let proc1 (row:'inputrow) : PGSQLConn<int> = 
        match dict.tryMakeHospitalRecord row with
        | Some vertex -> execNonQuery <| makeHospitalINSERT vertex
        | None -> pgsqlConn.Return 0
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 outfalls


let SetupHospitalDB (dict:HospitalInsertDict<'inputrow>) (hospitals:seq<'inputrow>) : Script<int> = 
    scriptMonad { 
        let! _ = deleteAllData () |> logScript (sprintf "%i rows deleted")
        let! count = insertHospitals dict hospitals  |> logScript (sprintf "%i rows inserted") 
        return count
     }

// OLD ******************

type HospitalList = HospitalRecord list


let private buildHospitalList () = 
    let hospitalData = new HospitalsData()
    let make1 (rowi : HospitalsRow) : HospitalRecord option = 
        match rowi.Name with 
        | null -> None
        | _ -> let optPt = Option.map osgb36ToWGS84 <| tryReadOSGB36Point rowi.``Grid Reference``
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

type BestMatch = { NearestHospital : HospitalRecord; DistanceToNearest: float<kilometer> }

let private tryClosestHosiptal (hospitals:HospitalList) (pt:WGS84Point) : BestMatch option =
    let find1 (dist,best) (hospital:HospitalRecord) = 
        let dist1 = haversineDistance pt hospital.LatLon
        if dist1 <= dist then
            (dist1, Some hospital)
        else (dist,best)
    List.fold find1 (50000.0<kilometer>, None) hospitals 
        |> fun (d,o) -> match o with 
                        | Some hosp -> Some { NearestHospital = hosp; DistanceToNearest = d }
                        | None -> None



type NearestHospitalDict<'asset> = 
    { TableHeaders      : option<string list> 
      ExtractLocation   : 'asset -> WGS84Point option
      OutputRow         : 'asset -> BestMatch option -> ClosedXMLOutput<unit> }
        
// TODO - change to Csv...
let generateNearestHospitalsXls (dict:NearestHospitalDict<'asset>) (source:'asset list) (outputFile:string) : unit =
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
