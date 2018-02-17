﻿module Scripts.NearestHospital

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


// Hard dependency on input data (though this file is in the project and Git)
type HospitalsData = 
    CsvProvider< @"..\data\Accident-and-Emergency-Hospitals-Yorkshire.csv",
                 HasHeaders = true>

type HospitalsRow = HospitalsData.Row


let getHospitalImportRows () : seq<HospitalsRow> = 
    (new HospitalsData ()).Rows |> Seq.cast<HospitalsRow>



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
    sqlINSERT "spt_hospitals" 
        <|  [ stringValue       "name"              hospital.HospitalName
            ; stringValue       "telephone"         hospital.Phone
            ; stringValue       "address"           hospital.Address
            ; stringValue       "postcode"          hospital.Postcode
            ; literalValue      "grid_ref"          <| makeSTGeogFromTextPointLiteral hospital.LatLon
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




type NeighbourRec = 
    { Name: string
      Telephone: string 
      Address: string
      Postcode: string
      GridRef: WGS84Point } 

type BestMatch = 
    { NearestHospital: NeighbourRec
      Distance: float<kilometer> }


let nearestHospitalQuery (point:WGS84Point) : Script<NeighbourRec list> = 
    let query = makeNearestNeighbourQUERY 1 point
    let procM (reader:NpgsqlDataReader) : NeighbourRec = 
        let gridRef = 
            match Option.bind wktPointToWGS84 <| tryReadWktPoint (reader.GetString(4)) with
            | Some pt -> pt
            | None -> failwith "findVertices ..."
        { Name          = reader.GetString(0)
        ; Telephone     = reader.GetString(1)
        ; Address       = reader.GetString(2) 
        ; Postcode      = reader.GetString(3)
        ; GridRef       = gridRef }
    liftWithConnParams << runPGSQLConn <| execReaderList query procM  

let nearestHospital (point:WGS84Point) : Script<NeighbourRec option> = 
    let first xs = match xs with | x :: _ -> Some x; | [] -> None
    fmapM first <| nearestHospitalQuery point



type NearestHospitalDict<'asset> = 
    { CsvHeaders        : string list
      ExtractLocation   : 'asset -> WGS84Point option
      OutputCsvRow      : 'asset -> BestMatch option -> SL.CsvOutput.RowWriter }


let generateNearestHospitalsCsv (dict:NearestHospitalDict<'asset>) (source:'asset list) (outputFile:string) : Script<unit> =
    let rowProc (asset1:'asset) : Script<SL.CsvOutput.CellWriter list> =
        scriptMonad { 
            let! optBest = 
                match dict.ExtractLocation asset1 with
                | Some assetLoc -> 
                    scriptMonad { 
                        let! optHospital = nearestHospital assetLoc
                        match optHospital with
                        | None -> return None
                        | Some hospital1 -> 
                            let! distance = pgDistanceSpheroid assetLoc hospital1.GridRef
                            return (Some { NearestHospital = hospital1; Distance=distance })
                        }
                | None -> scriptMonad.Return None
            return (dict.OutputCsvRow asset1 optBest)
        }
    
    scriptMonad { 
        let! (rowWriters:seq<SL.CsvOutput.RowWriter>) = SL.ScriptMonad.traverseM rowProc source
        let csvProc:CsvOutput<unit> = 
            SL.CsvOutput.writeRowsWithHeaders dict.CsvHeaders rowWriters
        do! liftAction <| SL.CsvOutput.outputToNew {Separator=","} csvProc outputFile
        }          

        

