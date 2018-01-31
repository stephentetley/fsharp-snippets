namespace SL.Geo

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open FParsec

open SL.Tolerance
open SL.Geo.Coord


module WellKnownText = 
    
    // Note - Wkt should not favour the WGS84 reference system.
    // Other spatial references with "Lon" & "Lat" are possible.
    
    
    // ** SRIDs for Phantom types


    /// World Geodetic System 1984             
    /// The SRID for this system is ESPG:4326
    type WGS84 = class end

    /// Ordinance Survey Great Britain National Grid reference system 
    /// The SRID for this system is ESPG:27700
    type OSGB36 = class end


    /// The undefined SRID.
    type private NoSRID = class end


    // Probably need: 
    // type WellKnownText<'a> = WKT of string
    // ... to represent answers from PostGIS


    // The base point type does not have a phantom type wrapper.
    // This means the phantom param is only wrapped once for LINESTRING etc.

    type WktCoord = 
        { WktLon: decimal      
          WktLat: decimal }


    /// Encode coordinate reference system as a phantom type.
    /// Values are represented as decimal
    type WktPoint<'a> = WktPoint of WktCoord

    let inline unwrapWktPoint (pt:WktPoint<'a>) : WktCoord = 
        match pt with | WktPoint pt -> pt
    
    let wktCoordsEqual (tx:Tolerance) (p1:WktCoord) (p2:WktCoord) : bool =
        tEqual tx p1.WktLon p2.WktLon && tEqual tx p1.WktLat p2.WktLat

    let wktPointsEqual (tx:Tolerance) (p1:WktPoint<'a>) (p2:WktPoint<'a>) : bool =
        wktCoordsEqual tx (unwrapWktPoint p1) (unwrapWktPoint p2)


    type WktLineString<'a> = WktLineString of WktCoord list 

    let inline unwrapWktLineString (source:WktLineString<'a>) : WktCoord list = 
        match source with | WktLineString xs -> xs
        

    type WktPolygon<'a> = WktPolygon of WktCoord list 

    let inline unwrapWktPolygon (source:WktPolygon<'a>) : WktCoord list = 
        match source with | WktPolygon xs -> xs

    let inline showWktCoord (coord:WktCoord) : string = 
        sprintf "%.5f %.5f" coord.WktLon coord.WktLat


    /// Prints as 'POINT(14.12345, 15.12345)'
    /// (Ideally we would print with user supplied precision)
    let inline showWktPoint (point:WktPoint<'a>) : string = 
        sprintf "POINT(%s)" (showWktCoord <| unwrapWktPoint point)


    /// Prints as 'LINESTRING(-1.08066 53.93863,-1.43627 53.96907)'
    let showWktLineString (source:WktLineString<'a>) : string =
        match unwrapWktLineString source with
        | [] -> "LINESTRING EMPTY"
        | xs -> sprintf "LINESTRING(%s)" (String.concat "," <| List.map showWktCoord xs)


    /// This is a simple closed polygon without any interior polygons.
    /// The user is responsible to ensure the polygon is closed before printing and
    /// that points are in counter-clockwise direction.
    let showWktPolygon (source:WktPolygon<'a>) : string =
        match unwrapWktPolygon source with
        | [] -> "POLYGON EMPTY"
        | xs -> sprintf "POLYGON(%s)" (String.concat "," <| List.map showWktCoord xs)  


    let wgs84PointToWKT (point:WGS84Point) : WktPoint<WGS84> =
        WktPoint <| { WktLon = decimal point.Longitude; WktLat = decimal point.Latitude }

    let osgb36PointToWKT (point:OSGB36Point) : WktPoint<OSGB36> = 
        WktPoint <| { WktLon = decimal point.Easting; WktLat = decimal point.Northing }

    let wktToWGS84Point (point:WktPoint<WGS84>) : WGS84Point = 
        match unwrapWktPoint point with
        | coord -> 
            { Latitude = 1.0<degree> * float coord.WktLat
            ; Longitude = 1.0<degree> * float coord.WktLon }

    let wktToOSGB36Point (point:WktPoint<OSGB36>) : OSGB36Point = 
        match unwrapWktPoint point with
        | coord -> 
            { Easting = 1.0<meter> * float coord.WktLon
            ; Northing = 1.0<meter> * float coord.WktLat }

    let wktOSGB36ToWktWGS84 (point:WktPoint<OSGB36>) : WktPoint<WGS84> = 
        point |> wktToOSGB36Point |> osgb36ToWGS84 |> wgs84PointToWKT

    let wktWGS84ToWktOSGB36 (point:WktPoint<WGS84>) : WktPoint<OSGB36> = 
        point |> wktToWGS84Point |> wgs84ToOSGB36 |> osgb36PointToWKT

    // ***** PARSING *****

    let private pSymbol (s:string) : Parser<string,'u> = 
        pstring s .>> spaces

    let private pParens (p:Parser<'a,'u>) : Parser<'a,'u> =
        between (pSymbol "(") (pSymbol ")") p

    // We have got over the value restriction by fixing the WktPoint phantom tag 
    // to TEMP and parser state to unit.
    // This is not a brilliant solution.

    let private pDecimal : Parser<decimal,unit> = pfloat |>> decimal

    let private pWktCoord : Parser<WktCoord, unit> = 
        pipe2   (pDecimal .>> spaces)
                (pDecimal .>> spaces) 
                (fun lon lat -> { WktLon = lon; WktLat = lat })

    // Utility combinators
    let private parsePOINT : Parser<WktPoint<NoSRID>, unit> = 
        let p1 = pWktCoord |>> WktPoint
        pSymbol "POINT" >>. pParens p1

    let private unTEMP (pt:WktPoint<NoSRID>) : WktPoint<'a> = 
        match pt with
        | WktPoint a -> (WktPoint a):WktPoint<'a>

    let tryReadWktPoint (source:string) : WktPoint<'a> option = 
        let ans1 = runParserOnString parsePOINT () "none" source
        match ans1 with
        | Success(a,_,_) -> Some <| unTEMP a
        | Failure(s,_,_) -> None

    // OLD CODE...

    let inline private parens (s:string) : string = sprintf "(%s)" s

    
    let inline private printPoint (coord:Coord.WGS84Point) : string = 
        sprintf "%.5f %.5f" coord.Longitude coord.Latitude

    let inline private printPointList (coords:Coord.WGS84Point list) : string = 
        String.concat ", " <| List.map printPoint coords
    
    let inline private printPointListParens (coords:Coord.WGS84Point list) : string = 
        String.concat ", " <| List.map (parens << printPoint) coords

    let inline private printListOfPointLists (listoflists:Coord.WGS84Point list list) = 
        let nonempties = List.filter (not << List.isEmpty) listoflists
        String.concat ", " <| List.map (parens << printPointList) nonempties 


    let private closePolygon (coords:Coord.WGS84Point list) : Coord.WGS84Point list = 
        match coords with
        | [] -> []
        | (hd::xs) -> 
            let rec proc ac rest = 
                match rest with
                | [] -> List.rev ac
                | [y] -> 
                    if y = hd then List.rev (y::ac) else List.rev (hd::y::ac)
                | (y::ys) -> proc (y::ac) ys
            proc [hd] xs 
    
    // Note - don't quote output, client code can do that if necessary

    let genPOINT (coord:Coord.WGS84Point) : string = 
        sprintf  "POINT(%f %f)" coord.Longitude coord.Latitude

    let genLINESTRING (coords:Coord.WGS84Point list) : string =
        match coords with
        | [] -> "LINESTRING EMPTY"
        | _ -> sprintf "LINESTRING(%s)" (printPointList coords)

    // User must ensure points are in counter-clockwise direction
    let genPOLYGON1 (coords:Coord.WGS84Point list) : string =
        match coords with
        | [] -> "POLYGON EMPTY"
        | _ -> 
            let closedlist = closePolygon coords
            sprintf "POLYGON(%s)" (printPointList closedlist)       

    // User must ensure exterior points are in counter-clockwise direction
    // and interior polygon points are in clockwise direction.
    let genPOLYGON (exterior:Coord.WGS84Point list) (interiors:Coord.WGS84Point list list) : string =
        match exterior with
        | [] -> "POLYGON EMPTY"
        | _ -> 
            let closeExt = closePolygon exterior
            let closedInts = List.map closePolygon interiors
            match closedInts with
            | [] -> sprintf "POLYGON(%s)" (printPointList closeExt)   
            | _ -> 
                sprintf "POLYGON((%s), %s)" (printPointList closeExt) (printListOfPointLists closedInts)
                    
    let genMULTIPOINT (coords:Coord.WGS84Point list) : string =
        match coords with
        | [] -> "MULTIPOINT EMPTY"
        | _ -> sprintf "MULTIPOINT(%s)" (printPointList coords)

    // TODO MULTILINESTRING MULTIPOLYGON and reading...

