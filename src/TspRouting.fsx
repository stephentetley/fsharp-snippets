#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

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


#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load "ClosedXMLWriter.fs"
open ClosedXMLWriter

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
    // Use direct recursion because not every step might be productive (so not mapAccumL [List.mapFold])
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

let pgInitializeTable : PGSQLConn<int> = 
    execNonQuery "TRUNCATE TABLE temp_routing;"


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
    let procM (reader:NpgsqlDataReader) = 
        seq { 
            while reader.Read() do 
                let (orec:DbRecord) = 
                    { Index     = int <| reader.GetInt64(2)
                      SiteCode  = reader.GetString(3)
                      LongName  = reader.GetString(4)
                      Wgs84Lat  = float <| reader.GetDouble(5)
                      Wgs84Lon  = float <| reader.GetDouble(6) }
                yield orec} |> Seq.toList
            // printfn "seq:%i code:%s" (reader.GetInt64(0)) (reader.GetString(3)) 
    execReader query procM 

let outputXslx (records:DbRecord list) (fileName:string) : unit = 
    let proc1 (orec:DbRecord) (ix:int) : ClosedXMLWriter<unit> = 
        tellRow   [ (ix + 1).ToString()
                  ; orec.SiteCode
                  ; orec.LongName
                  ; orec.Wgs84Lat.ToString()
                  ; orec.Wgs84Lon.ToString() ]
    let procM = 
        closedXMLWriter { do! tellHeaders ["Order"; "Code"; "Name"; "Latitude"; "Longitude"]
                          do! foriMz records proc1 } 
    outputToNew procM fileName "Routes"


let main (pwd:string) : unit = 
    let outputPath= @"G:\work\Projects\pgrouting\routing_output.xlsx"
    let records = makeDBRecords <| readInputJson @"G:\work\Projects\pgrouting\routing_data1.json"
    let conn = makeConnString pwd "spt_geo"
    let procM = pgsqlConn { let! _   = pgInitializeTable
                            let! ans = pgInsertRecords records 
                            return ans }
    match runPGSQLConn procM conn with
    | Err(msg) -> printfn "ERR: %s" msg
    | Ok(i) -> 
        match (tryFindFurthestNorth records, tryFindFurthestSouth records) with
        | (Some(north) , Some(south)) ->
            match runPGSQLConn (pgTSPQuery north south) conn with
            | Err(msg) -> printfn "ERR: %s" msg
            | Ok(results) -> outputXslx results outputPath
        | _ -> printfn "Err - no north and south"




///////////////////////////////////////////////////////////////////////////////

/// OLD 


// TODO - we should not be tied to the type provider of a particular spreadsheet. 
// Ideally input should be Json or something already in the form of List<Node>.

type RoutingTable = 
    ExcelFile< @"G:\work\Projects\pgrouting\Erskine Site List.xlsx",
               SheetName = "Site List",
               ForceString = true >

type RoutingRow = RoutingTable.Row

// TODO rowi.NGR causes what initially appears very obscure error location/message
// if it is null.
let makeNode (ix:int) (rowi:RoutingRow) : DbRecord option = 
    let ngr = 
        match rowi.NGR with
        | null -> "ERROR"    
        | value -> value      
    let mk1 (pt:Coord.WGS84Point) : DbRecord = 
        { Index = ix
          SiteCode = rowi.``SAI Number``
          LongName = rowi.``Site Name``
          Wgs84Lat = float pt.Latitude
          Wgs84Lon = float pt.Longitude }
    Option.map (mk1 << Coord.osgb36GridToWGS84) <| Coord.tryReadOSGB36Grid ngr
    
let makeNodeList () : DbRecord list = 
    let routingData = new RoutingTable()
    let make1 (i:int) (rowi:RoutingRow) : (DbRecord option * int) = 
        match rowi.``SAI Number`` with
        | null -> (None,i)
        | _ -> 
            let optPt = makeNode i rowi
            match optPt with
            | None -> (None,i) 
            | Some node -> (Some node, i+1)
    match routingData.Data with
    // | null -> failwith "Mynull"
    | aseq -> aseq 
                |> Seq.mapFold make1 1
                |> fst
                |> Seq.toList
                |> List.choose id

let findIndex (nodes: DbRecord list) (longName:string) : int option = 
    Option.map (fun o -> o.Index)
        <| List.tryFind (fun node -> longName = node.LongName) nodes




// Change to use explicit field names as that is more robust:
// INSERT INTO temp_routing (id, point_code, ...) VALUES (1, 'Z001', 'MAYBURY', ...);
//
let genINSERT (sb:System.Text.StringBuilder) (tableName: string) (nodes: DbRecord list) : unit =
    List.iter 
        (fun node -> 
            Printf.bprintf sb "INSERT INTO %s VALUES (%d,'%s','%s',%f,%f);\n" 
                tableName
                node.Index
                node.SiteCode
                node.LongName
                node.Wgs84Lat
                node.Wgs84Lon)
        nodes



// No need to CREATE TABLE - do that within PostgreSQL...
let genSQL (nodes: DbRecord list) (tableName:string)  (first:int) (last:int) : string = 
    let sb = System.Text.StringBuilder ()
    Printf.bprintf sb "-- DROP TABLE %s;\n\n" tableName
    Printf.bprintf sb "CREATE TABLE %s (\n" tableName
    ignore <| sb.AppendLine "    id integer NOT NULL,"
    ignore <| sb.AppendLine "    codename character varying(30) NOT NULL,"
    ignore <| sb.AppendLine "    longname character varying(150) NOT NULL,"
    ignore <| sb.AppendLine "    lat double precision,"
    ignore <| sb.AppendLine "    lon double precision"
    ignore <| sb.AppendLine ");\n"
    Printf.bprintf sb "ALTER TABLE ONLY %s\n" tableName
    Printf.bprintf sb "    ADD CONSTRAINT pk_%s_id PRIMARY KEY (id);\n\n" tableName

    ignore <| sb.AppendLine "BEGIN;"
    genINSERT sb tableName nodes
    ignore <| sb.AppendLine "COMMIT;\n"

    ignore <| sb.AppendLine ""
    ignore <| sb.AppendLine "SELECT seq, t.id1, p.id, p.codename, p.longname, p.lat, p.lon"
    ignore <| sb.AppendLine "FROM"
    ignore <| sb.AppendLine "	pgr_tsp("
    ignore <| sb.AppendLine "		'SELECT id, lon as x, lat as y"
    Printf.bprintf sb  "		FROM %s',\n" tableName
    Printf.bprintf sb  "		%d,\n" first
    Printf.bprintf sb  "		%d\n" last
    ignore <| sb.AppendLine "	) As t"
    ignore <| sb.AppendLine "	INNER JOIN"
    Printf.bprintf sb   "	%s As p\n" tableName
    ignore <| sb.AppendLine "    ON t.id2 = p.id"
    ignore <| sb.AppendLine "ORDER BY seq;"
    sb.ToString ()


let test01 () = 
    let routingData = new RoutingTable()
    for (rowi:RoutingRow) in routingData.Data do
        match rowi.``SAI Number`` with
        | null -> printfn "<finished>"
        | _ -> printfn "%s, %s" rowi.``SAI Number`` rowi.``Site Name``


let outpath = @"G:\work\Projects\pgrouting\outputbuffer.sql"
let mainOLD () = 
    let nodes = makeNodeList ()
    let start = Option.get <| tryFindFurthestNorth nodes
    let final = Option.get <| tryFindFurthestSouth nodes
    let sql = genSQL nodes "batteries" start.Index final.Index
    System.IO.File.WriteAllText (outpath, sql)
    printfn "%s" sql
