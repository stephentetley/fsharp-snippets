module Scripts.NearestHospital

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open FSharp.Data
open Npgsql

open SL.AnswerMonad
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.SqlUtils
open SL.CsvOutput
open SL.PGSQLConn
open SL.ScriptMonad
open SL.PostGIS

// Use PostGIS for nearest neighour and distance.


/// Hard dependency on input data (though this file is in the project and Git)
/// Another user could set up the DB with a different data set.
type HospitalsData = 
    CsvProvider< @"..\data\Accident-and-Emergency-Hospitals-Yorkshire.csv",
                 HasHeaders = true>

type HospitalsRow = HospitalsData.Row


let getHospitalImportRows () : seq<HospitalsRow> = 
    (new HospitalsData ()).Rows |> Seq.cast<HospitalsRow>



let deleteAllData () : Script<int> = 
    liftPGSQLConn <| deleteAllRowsRestartIdentity "spt_hospitals"



[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type HospitalRecord = 
    { HospitalName: string
      Address: string
      Phone: string
      Postcode: string
      GridRef: WGS84Point }

type HospitalInsertDict<'inputrow> = 
    { tryMakeHospitalRecord : 'inputrow -> HospitalRecord option }

let tryMakeRecord (row:HospitalsRow) : HospitalRecord option = 
    match tryReadOSGB36Point row.``Grid Reference`` with
    | Some osgb36 -> 
        Some <| { HospitalName  = row.Name
                ; Address       = row.Address
                ; Phone         = row.Telephone
                ; Postcode      = row.Postcode
                ; GridRef       = osgb36ToWGS84 osgb36 }
    | _ -> None

let MakeDict : HospitalInsertDict<HospitalsRow> = { tryMakeHospitalRecord = tryMakeRecord }


let private makeHospitalINSERT (hospital:HospitalRecord) : string = 
    sqlINSERT "spt_hospitals" 
        <|  [ stringValue       "name"              hospital.HospitalName
            ; stringValue       "telephone"         hospital.Phone
            ; stringValue       "address"           hospital.Address
            ; stringValue       "postcode"          hospital.Postcode
            ; literalValue      "grid_ref"          <| makeSTGeogFromTextPointLiteral hospital.GridRef
            ]


let insertHospitals (dict:HospitalInsertDict<'inputrow>) (outfalls:seq<'inputrow>) : Script<int> = 
    let proc1 (row:'inputrow) : PGSQLConn<int> = 
        match dict.tryMakeHospitalRecord row with
        | Some vertex -> execNonQuery <| makeHospitalINSERT vertex
        | None -> pgsqlConn.Return 0
    liftPGSQLConn << withTransaction <| SL.PGSQLConn.sumTraverseM proc1 outfalls


let SetupHospitalDB (dict:HospitalInsertDict<'inputrow>) (hospitals:seq<'inputrow>) : Script<int> = 
    scriptMonad { 
        let! _      = deleteAllData ()                  |> logScript (sprintf "%i rows deleted")
        let! count  = insertHospitals dict hospitals    |> logScript (sprintf "%i rows inserted") 
        return count
     }


/// Limit is closest N neighbours, probably should be fixed to 1
/// for this use-case.
let makeNearestNeighbourQUERY (limit:int) (point:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            name, telephone, address, postcode, ST_AsText(grid_ref)
        FROM 
            spt_hospitals 
        ORDER BY grid_ref <-> ST_Point({0}, {1}) LIMIT {2} ;
        """, point.Longitude, point.Latitude, limit)




type BestMatch = 
    { NearestHospital: HospitalRecord
      Distance: float<kilometer> }


let nearestHospitalQuery (point:WGS84Point) : Script<HospitalRecord list> = 
    let query = makeNearestNeighbourQUERY 1 point
    let procM (reader:NpgsqlDataReader) : HospitalRecord = 
        let gridRef = 
            match Option.bind wktPointToWGS84 <| tryReadWktPoint (reader.GetString(4)) with
            | Some pt -> pt
            | None -> failwith "nearestHospitalQuery - invalid gridRef ..."
        { HospitalName  = reader.GetString(0)
        ; Phone         = reader.GetString(1)
        ; Address       = reader.GetString(2) 
        ; Postcode      = reader.GetString(3)
        ; GridRef       = gridRef }
    liftPGSQLConn <| execReaderList query procM  


let nearestHospitalToPoint (point:WGS84Point) : Script<HospitalRecord option> = 
    let first xs = match xs with | x :: _ -> Some x; | [] -> None
    fmapM first <| nearestHospitalQuery point



let nearestHospital (extractLoc:'asset -> WGS84Point option) (asset:'asset) : Script<HospitalRecord> = 
    runOptional "nearestHospital fail" 
        <| scriptMonad { 
            match extractLoc asset with
            | None -> return None
            | Some pt -> 
                let! ans = nearestHospitalToPoint pt 
                return ans
            }
    
let nearestHospitals (extractLoc:'asset -> WGS84Point option) (assets:seq<'asset>) : Script<seq<'asset * HospitalRecord option>> = 
    let proc1 (asset:'asset) = 
        scriptMonad { 
            let! opt = optional <| nearestHospital extractLoc asset
            return asset, opt
            }
    traverseM proc1 assets
    

// "End-to-end" procedure for finding and outputing nearest hospitals.
// We don't set up the DB as we expect the DB to be long lived.

type NearestHospitalDict<'asset> = 
    { CsvHeaders: string list
      ExtractLocation: 'asset -> WGS84Point option
      OutputCsvRow: 'asset -> BestMatch option -> RowWriter }



let generateNearestHospitalsCsv (dict:NearestHospitalDict<'asset>) (source:'asset list) (outputFile:string) : Script<unit> =
    
    let rowProc (asset1:'asset, opt:HospitalRecord option) : Script<RowWriter> =
        let noBest = dict.OutputCsvRow asset1 None
        replaceFailure noBest <| scriptMonad { 
            let! hospital   = liftOption opt
            let! assetLoc   = liftOption <| dict.ExtractLocation asset1
            let! distance   = pgDistanceSpheroid assetLoc hospital.GridRef
            let best        = Some <| { NearestHospital = hospital; Distance=distance }
            return (dict.OutputCsvRow asset1 best)
        }

    scriptMonad { 
        let! xs = nearestHospitals dict.ExtractLocation source
        let! (rowWriters:seq<RowWriter>) = traverseM rowProc xs
        let csvProc:CsvOutput<unit> = writeRowsWithHeaders dict.CsvHeaders rowWriters
        do! liftAction <| SL.CsvOutput.outputToNew {Separator=","} csvProc outputFile
    }



