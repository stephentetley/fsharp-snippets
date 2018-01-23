#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql


#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\Coord.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.Geo.Coord
open SL.PGSQLConn
open SL.ScriptMonad
open SL.CsvOutput


type GisOutfallData = 
    CsvProvider< @"G:\work\Projects\events2\db-import-tables\gis-outlets-wkt.csv",
                 HasHeaders = true>

type GisOutfallRow = GisOutfallData.Row

let getGisOutfalls () : GisOutfallRow list = (new GisOutfallData ()).Rows |> Seq.toList

type NeighboursData = 
    CsvProvider< @"G:\work\Projects\events2\Asset-collected-data.csv",
                 HasHeaders = true>

type NeighboursRow = NeighboursData.Row

let getDataForNeighbours () : seq<NeighboursRow> = (new NeighboursData ()).Rows |> Seq.cast<NeighboursRow>


// ********** SCRIPT **********
type Script<'a> = ScriptMonad<PGSQLConnParams,'a>

let withConnParams (fn:PGSQLConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftWithConnParams (fn:PGSQLConnParams -> Answer<'a>) : Script<'a> = 
    withConnParams <| (liftAnswer << fn)

let deleteAllData () : Script<int> = 
    liftWithConnParams << runPGSQLConn <| deleteAllRows "spt_outfalls"

let makeOutfallINSERT (row:GisOutfallRow) : string = 
    let east    = 1.0<meter> * (float <| row.METREEASTING)
    let north   = 1.0<meter> * (float <| row.METRENORTHING)
    let wgs84Pt = osgb36PointToWGS84 <| { Easting = east; Northing = north }
    let pointLit = 
        sprintf "ST_GeogFromText('SRID=4326;POINT(%f %f)')"
                wgs84Pt.Longitude wgs84Pt.Latitude

    sqlINSERT "spt_outfalls" 
        <|  [ stringValue       "stc25_ref"         row.STC25_REF
            ; stringValue       "function_node"     row.FUNCTION_NODE
            ; literalValue      "point_loc"         pointLit
            ]



let insertOutfalls () : Script<int> = 
    let rows = getGisOutfalls ()
    let proc1 (row:GisOutfallRow) : PGSQLConn<int> = execNonQuery <| makeOutfallINSERT row
    liftWithConnParams 
        << runPGSQLConn << withTransaction <| SL.PGSQLConn.sumForM rows proc1



let SetupDB(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
  
    runScript (failwith) (printfn "Success: %i modifications") (consoleLogger) conn 
        <| sumSequenceM 
            [ deleteAllData ()          |> logScript (sprintf "%i rows deleted")
            ; insertOutfalls ()         |> logScript (sprintf "%i rows inserted") 
            ]

/// Limit is closest N neighbours
let makeNearestNeighbourQUERY (limit:int) (point:WGS84Point) : string = 
    System.String.Format("""
        SELECT 
            stc25_ref, function_node 
        FROM 
            spt_outfalls 
        ORDER BY point_loc <-> ST_Point({0}, {1}) LIMIT {2} ;
        """, point.Longitude, point.Latitude, limit)

type NeighbourRec = 
    { STC25Ref: string
      FunctionNode: string }

let pgNearestNeighbourQuery (limit:int) (point:WGS84Point) : PGSQLConn<NeighbourRec list> = 
    let query = makeNearestNeighbourQUERY limit point
    let procM (reader:NpgsqlDataReader) : NeighbourRec = 
        { STC25Ref  = reader.GetString(0)
          FunctionNode  = reader.GetString(1) }
    execReaderList query procM   




let printNeighbours (recs:NeighbourRec list) : string = 
    String.concat " & " 
        <| List.map (fun (x:NeighbourRec) -> x.STC25Ref) recs

type OutputRow = 
    { Sai: string
      Name: string
      CatsNGRs: string
      OutfallNNs: string }


let genOutputRow (limit:int) (row:NeighboursRow) : Script<OutputRow> = 
    let neighbours () = 
        match Option.map osgb36GridToWGS84 <| tryReadOSGB36Grid row.``Cats NGRs`` with
        | Some wgs84 -> 
            scriptMonad { 
                let! ansList = liftWithConnParams << runPGSQLConn <| pgNearestNeighbourQuery 5 wgs84
                return (printNeighbours ansList)
                }
        | None -> scriptMonad.Return "??"
    scriptMonad { 
        let! neighboursText = neighbours ()
        return { Sai = row.``SAI Number``
               ; Name = row.``Asset Name``
               ; CatsNGRs = row.``Cats NGRs``
               ; OutfallNNs = neighboursText }
    }           


let tellOutputRow (row:OutputRow) : CellWriter list = 
    [ tellString row.Sai
    ; tellString row.Name
    ; tellString row.CatsNGRs
    ; tellString row.OutfallNNs ]

let csvHeaders =  [ "SAI Number"; "Asset Name"; "Cats NGRs"; "Outfall NNs"]

let OutputNN(password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    let outFile = @"G:\work\Projects\events2\outfall-neighbours.csv"
    // Ideally CsvOutput should have the same extent as ScriptMonad... 
    runScript (failwith) (printfn "Success: %A") (consoleLogger) conn 
        <| scriptMonad { 
                let (rows1:seq<NeighboursRow>) = getDataForNeighbours ()
                let! (rows2:seq<OutputRow>) = SL.ScriptMonad.traverseM (genOutputRow 5) rows1
                let csvProc = tellSheetWithHeaders csvHeaders rows2 tellOutputRow
                ignore << liftAction <| outputToNew csvProc outFile
                }
