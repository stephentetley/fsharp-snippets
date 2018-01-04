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
#load "ClosedXMLWriter.fs"
open ClosedXMLWriter


#I @"..\packages\Npgsql.3.2.6\lib\net451\"
#I @"..\packages\System.Threading.Tasks.Extensions.4.3.0\lib\portable-net45+win8+wp8+wpa81"
#r "Npgsql"
open Npgsql

#load "Geo.fs"
open Geo
#load "ResultMonad.fs"
open ResultMonad
#load "SqlUtils.fs"
open SqlUtils
#load "PGSQLConn.fs"
open PGSQLConn


#I @"..\packages\Newtonsoft.Json.10.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json
#load "JsonOutput.fs"
open JsonOutput


// Use PostGIS's pgr_tsp function
// This was written to generate a sql file that could be 
// loaded/run at PostgreSQL's command line.
// It should be reworked to use Npgsql / PGSQLConn.

// Implementation note:
// PostGIS (pgr_tsp) seems to like (or need) a numeric
// id on the coordinate table.



let makeConnString (pwd:string) (dbname:string) : string = 
    let fmt : Printf.StringFormat<(string -> string -> string)> = "Host=localhost;Username=postgres;Password=%s;Database=%s";
    sprintf fmt pwd dbname


type DbRecord = 
    { Index : int
      SiteCode : string 
      LongName : string
      Wgs84Lat : float
      Wgs84Lon : float }

type JsonRecord =
    { Uid: string
      Name: string
      Osgb36NGR: string }

// The Json input file is an array of JsonRecords

// Structure is known!
// We have a JsonValue object which we can "tree parse".
let extractor (jsonValue:JsonValue) : JsonRecord list = 
    let extrObj (value:JsonValue) : JsonRecord = 
        { Uid = value.["UID"].AsString()
          Name = value.["Name"].AsString()
          Osgb36NGR = value.["OSGB36NGR"].AsString() }
    [ for v in jsonValue -> extrObj v ]

let readInputJson (fileName:string) : JsonRecord list = 
    fileName 
        |> System.IO.File.ReadAllText
        |> JsonValue.Parse 
        |> extractor 

// let testSOFAR () = readInputJson @"G:\work\Projects\pgrouting\routing_data1.json"

let tryMakeDbRecord (ix:int) (input:JsonRecord) : DbRecord option = 
    let proc (ngr:Coord.OSGB36Grid) : DbRecord =  
        let wgs84 = Coord.osgb36GridToWGS84 ngr
        { Index= ix; SiteCode = input.Uid; LongName = input.Name; 
          Wgs84Lat = float wgs84.Latitude; 
          Wgs84Lon = float wgs84.Longitude }
    Option.map proc (Coord.tryReadOSGB36Grid <| input.Osgb36NGR) 


let makeDBRecords (inputs:JsonRecord list) : DbRecord list = 
    // Use direct recursion because not every step might be productive (so not mapAccumL [aka List.mapFold])
    // Start count at 1
    let rec proc (ix:int) (ac:DbRecord list) (ins:JsonRecord list) : DbRecord list = 
        match ins with
        | [] ->  List.rev ac
        | (x::xs) -> 
            match tryMakeDbRecord ix x with
            | Some(rec1) -> proc (ix+1) (rec1::ac) xs
            | None -> proc ix ac xs
    proc 1 [] inputs

// This is the new style...
let genINSERT1 (rec1:DbRecord) : string = 
    sqlINSERT "temp_routing" 
        <|  [ intValue      "id"            rec1.Index
            ; stringValue   "point_code"    rec1.SiteCode
            ; stringValue   "point_name"    rec1.LongName
            ; floatValue    "wgs84lat"      rec1.Wgs84Lat
            ; floatValue    "wgs84lon"      rec1.Wgs84Lon ]


let pgInsertRecords (records:DbRecord list) : PGSQLConn<int> = 
    PGSQLConn.fmapM (List.sum) <| withTransaction (PGSQLConn.forM records (execNonQuery  << genINSERT1))

let pgInitializeTable : PGSQLConn<int> = deleteAllRows "temp_routing;"


let tryFindFindNode(cmp : DbRecord -> DbRecord -> bool) (nodes: DbRecord list) : DbRecord option = 
    let find1 (ac: DbRecord option) (elem:DbRecord) = 
        match ac with
        | None -> Some elem
        | Some ac1 -> 
            if cmp elem ac1 then
                Some elem
            else ac
    List.fold find1 None nodes

let tryFindFurthestNorth (nodes: DbRecord list) : DbRecord option = 
    tryFindFindNode (fun elem ac ->  elem.Wgs84Lat > ac.Wgs84Lat) nodes

let tryFindFurthestSouth (nodes: DbRecord list) : DbRecord option = 
    tryFindFindNode (fun elem ac ->  elem.Wgs84Lat < ac.Wgs84Lat) nodes

let genTSPQuery (startPt:DbRecord) (endPt:DbRecord) : string = 
    System.String.Format("""
        SELECT seq, t.id1, p.id, p.point_code, p.point_name, p.wgs84lat, p.wgs84lon
        FROM
            pgr_tsp(
                'SELECT id, wgs84lon as x, wgs84lat as y
                FROM temp_routing',
                {0},
                {1}
            ) As t
            INNER JOIN
            temp_routing As p
            ON t.id2 = p.id
        ORDER BY seq;
        """, (startPt.Index), (endPt.Index))

// Force the seq to a List otherwise the connection appears to close with returning
// any values.
let pgTSPQuery (startPt:DbRecord) (endPt:DbRecord) : PGSQLConn<DbRecord list> = 
    let query = genTSPQuery startPt endPt
    let procM (reader:NpgsqlDataReader) : DbRecord = 
        { Index     = int <| reader.GetInt64(2)
          SiteCode  = reader.GetString(3)
          LongName  = reader.GetString(4)
          Wgs84Lat  = float <| reader.GetDouble(5)
          Wgs84Lon  = float <| reader.GetDouble(6) }
    execReaderList query procM          // for some reason execReaderList does not work and we have to inline it here...

let outputXslx (records:DbRecord list) (fileName:string) : unit = 
    let proc1 (ix:int) (orec:DbRecord) : RowWriter<unit> = 
        [ ClosedXMLWriter.tellInteger   (ix + 1)
        ; ClosedXMLWriter.tellString    orec.SiteCode
        ; ClosedXMLWriter.tellString    orec.LongName
        ; ClosedXMLWriter.tellFloat     orec.Wgs84Lat
        ; ClosedXMLWriter.tellFloat     orec.Wgs84Lon ]
    let procM = tellSheetWithHeadersi ["Order"; "Code"; "Name"; "Latitude"; "Longitude"] records proc1 
    outputToNew procM fileName "Routes"

let findStartAndEnd (findStart:DbRecord list -> DbRecord option) 
                    (findEnd:DbRecord list -> DbRecord option)
                    (records:DbRecord list) : Result<DbRecord*DbRecord> = 
    match (findStart records, findEnd records) with
        | (Some(start1) , Some(end1)) -> Ok(start1,end1)
        | (Some(_), None) -> Err "Cannot find end"
        | (None, Some(_)) -> Err "Cannot find start"
        | (_,_) -> Err "Cannot start and end records"

let main (pwd:string) : unit = 
    let outputPath= @"G:\work\Projects\pgrouting\routing_output.xlsx"
    let records = makeDBRecords <| readInputJson @"G:\work\Projects\pgrouting\routing_data1.json"
    let conn = pgsqlConnParamsTesting pwd "spt_geo"
    let procSetup = 
        pgsqlConn { let! _   = pgInitializeTable
                    let! ans = pgInsertRecords records 
                    return ans }
    let ans = 
        runResultWithError 
            <| resultMonad { 
                let! count1        = runPGSQLConn procSetup conn
                let! (start1,end1) = findStartAndEnd tryFindFurthestNorth tryFindFurthestSouth records
                let! results       = runPGSQLConn (pgTSPQuery start1 end1) conn
                do! liftAction (outputXslx results outputPath)  }
    printfn "%A" ans




///////////////////////////////////////////////////////////////////////////////

// Make input Json...

type ImportTable = 
    ExcelFile< @"G:\work\Projects\pgrouting\Erskine Site List.xlsx",
               SheetName = "Site List",
               ForceString = true >

type ImportRow = ImportTable.Row

let buildImports () : ImportRow list  =
    let importData = new ImportTable()
    let nullPred (row:ImportRow) = match row.GetValue(0) with null -> false | _ -> true
    importData.Data |> Seq.filter nullPred |> Seq.toList

let genJSON (rows:ImportRow list) : JsonOutput<unit> = 
    tellAsArray rows (fun (row:ImportRow) -> 
                        tellObject [ "UID",         tellString row.``SAI Number``
                                   ; "Name",        tellString row.``Site Name``
                                   ; "OSGB36NGR",   tellString row.NGR ] )

let main2 () : unit = 
    let outputPath = @"G:\work\Projects\pgrouting\routing_data1.json"
    let rows = buildImports ()
    ignore <| runJsonOutput (genJSON rows) 2 outputPath

