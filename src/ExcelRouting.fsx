#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#load "Geo.fs"
open Geo

#load "SqlUtils.fs"
open SqlUtils

// Use PostGIS's pgr_tsp function
// This was written to generate a sql file that could be 
// loaded/run at PostgreSQL's command line.
// It should be reworked to use Npgsql / PGSQLConn.

// Implementation note:
// PostGIS (pgr_tsp) seems to like (or need) a numeric
// id on the coordinate table.

type RoutingTable = 
    ExcelFile< @"G:\work\Projects\pgrouting\Erskine Site List.xlsx",
               SheetName = "Site List",
               ForceString = true >

type RoutingRow = RoutingTable.Row

type Node = 
    { index : int
      sitecode : string 
      longname : string
      lat : float
      lon : float }



// TODO rowi.NGR causes what initially appears very obscure error location/message
// if it is null.
let makeNode (ix:int) (rowi:RoutingRow) : Node option = 
    let ngr = 
        match rowi.NGR with
        | null -> "ERROR"    // Coord.fromOSGridRef10 will politely fail
        | value -> value      
    let mk1 (pt:Coord.WGS84Point) : Node = 
        { index = ix
          sitecode = rowi.``SAI Number``
          longname = rowi.``Site Name``
          lat = float pt.Latitude
          lon = float pt.Longitude }
    Option.map (mk1 << Coord.osgb36GridToWGS84) <| Coord.tryReadOSGB36Grid ngr
    
let makeNodeList () : Node list = 
    let routingData = new RoutingTable()
    let make1 (i:int) (rowi:RoutingRow) : (Node option * int) = 
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

let findIndex (nodes: Node list) (longName:string) : int option = 
    Option.map (fun o -> o.index)
        <| List.tryFind (fun node -> longName = node.longname) nodes

let findNodeIndex (cmp : Node -> Node -> bool) (nodes: Node list) : int option = 
    let find1 (ac: Node option) (elem:Node) = 
        match ac with
        | None -> Some elem
        | Some ac1 -> 
            if cmp elem ac1 then
                Some elem
            else ac
    Option.map (fun o -> o.index)  <| List.fold find1 None nodes

let furthestNorth (nodes: Node list) : int option = 
    findNodeIndex (fun elem ac ->  elem.lat > ac.lat) nodes

let furthestSouth (nodes: Node list) : int option = 
    findNodeIndex (fun elem ac ->  elem.lat < ac.lat) nodes


// Change to use explicit field names as that is more robust:
// INSERT INTO temp_routing (id, point_code, ...) VALUES (1, 'Z001', 'MAYBURY', ...);
//
let genINSERT (sb:System.Text.StringBuilder) (tableName: string) (nodes: Node list) : unit =
    List.iter 
        (fun node -> 
            Printf.bprintf sb "INSERT INTO %s VALUES (%d,'%s','%s',%f,%f);\n" 
                tableName
                node.index
                node.sitecode
                node.longname
                node.lat
                node.lon)
        nodes

// This is the new style...
let genINSERT1 (node:Node) : string = 
    sqlINSERT "temp_routing" 
        <|  [ intValue      "id"            node.index
            ; stringValue   "point_code"    node.sitecode
            ; stringValue   "point_name"    node.sitecode
            ; floatValue    "wgs84lat"      node.lat
            ; floatValue    "wgs84lon"      node.lon ]


let genSQL (nodes: Node list) (tableName:string)  (first:int) (last:int) : string = 
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
let main () = 
    let nodes = makeNodeList ()
    let start = Option.get <| furthestNorth nodes
    let final = Option.get <| furthestSouth nodes
    let sql = genSQL nodes "batteries" start final
    System.IO.File.WriteAllText (outpath, sql)
    printfn "%s" sql
