#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput

#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"


#load @"SL\AnswerMonad.fs"
#load @"SL\Coord.fs"
#load @"SL\Tolerance.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\PGSQLConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\ScriptMonad.fs"
open SL.Geo.Coord
open SL.AnswerMonad
open SL.SqlUtils
open SL.PGSQLConn
open SL.CsvOutput
open SL.ScriptMonad

#load @"Scripts\PostGIS.fs"
#load @"Scripts\RoutingTsp.fs"
open Scripts.RoutingTsp


let [<Literal>] StationsCsv  =  @"..\data\stations.csv"
type StationData = 
    CsvProvider< StationsCsv,
                 HasHeaders = true>

type StationRow = StationData.Row

let getStations () : StationRow list = (new StationData ()).Rows |> Seq.toList


let tspVertexInsertDict:TspVertexInsertDict<StationRow> = 
    { TryMakeVertexPoint = 
        fun (row:StationRow) -> Option.map osgb36ToWGS84 <| tryReadOSGB36Point row.Grid_Ref
      MakeVertexLabel = 
        fun (row:StationRow) -> row.Name
    }


let SetupDB(password:string) : unit = 
    let rows = getStations ()
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A") conn 
        <| SetupTspVertexDB tspVertexInsertDict rows 


let stationOutputDict : TspPrintRouteStepDict = 
    { CsvHeaders = [ "Serial Num"; "Station"; "Grid Ref"; "Aggregate Cost" ]
      MakeCsvRow =  
        fun (node:RouteNode) -> 
            [ tellInt           node.SeqNumber
            ; tellString        node.NodeLabel
            ; tellString        << showOSGB36Point << wgs84ToOSGB36 <| node.GridRef
            ; tellFloat         node.AggCost
            ]
    }


let main (password:string) : unit = 
    let outputFile = __SOURCE_DIRECTORY__ + @"\..\data\stations-route.csv"
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A ") conn 
        <| scriptMonad { 
            let! startId    = findIdByLabel "Bradford Interchange"
            let! endId      = findIdByLabel "Mytholmroyd"
            do! generateTspRouteCsv stationOutputDict startId endId outputFile
            }


let test02 (password:string) : unit = 
    let conn = pgsqlConnParamsTesting "spt_geo" password
    runConsoleScript (printfn "Success: %A ") conn 
        <| scriptMonad { 
            let! startId    = furthestEastId
            let! endId      = furthestWestId
            let! ans        = eucledianTSP startId endId 
            return ans
            }


