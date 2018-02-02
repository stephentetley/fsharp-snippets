﻿namespace SL.Geo

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open FParsec

open SL.Tolerance
open SL.Geo.Coord

// Only concerned with 2d.
// 3d or 4d would be the subject for another module.
// Note BNF starts on page 54 of the spec.
// This should be the authority for naming etc.

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
    // Represent answers from PostGIS
    type WellKnowntText<'a> = WellKnowntText of string


    /// The base point type does not have a phantom type wrapper.
    /// This means the phantom param is only wrapped once for LINESTRING etc.
    type WktCoord = 
        { WktLon: decimal      
          WktLat: decimal }


    /// Encode coordinate reference system as a phantom type.
    /// Values are represented as decimal
    /// A null point must be allowed, encode it internally with option.
    type WktPoint<'a> = WktPoint of WktCoord option

    let inline unwrapWktPoint (pt:WktPoint<'a>) : WktCoord option = 
        match pt with | WktPoint opt -> opt
    
    let wktCoordsEqual (tx:Tolerance) (p1:WktCoord) (p2:WktCoord) : bool =
        tEqual tx p1.WktLon p2.WktLon && tEqual tx p1.WktLat p2.WktLat

    // Null Point <> Null Point
    let wktPointsEqual (tx:Tolerance) (point1:WktPoint<'a>) (point2:WktPoint<'a>) : bool =
        match unwrapWktPoint point1, unwrapWktPoint point2 with
        | Some p1, Some p2 -> wktCoordsEqual tx p1 p2
        | _,_ -> false      

    type WktMultiPoint<'a> = WktMultiPoint of WktCoord list 

    let inline unwrapWktMultiPoint (source:WktMultiPoint<'a>) : WktCoord list = 
        match source with | WktMultiPoint xs -> xs

    type WktLineString<'a> = WktLineString of WktCoord list 

    let inline unwrapWktLineString (source:WktLineString<'a>) : WktCoord list = 
        match source with | WktLineString xs -> xs
        
        
    type WktPolygon<'a> =   
        { ExteriorRing: WktCoord list 
          InteriorRings: (WktCoord list) list }



    // ***** PRINTING *****
    
    let inline showWktCoord (coord:WktCoord) : string = 
        sprintf "%.5f %.5f" coord.WktLon coord.WktLat


    /// Prints as 'POINT(14.12345, 15.12345)'
    /// (Ideally we would print with user supplied precision)
    let inline showWktPoint (point:WktPoint<'a>) : string = 
        match unwrapWktPoint point with
        | None -> "POINT EMPTY"
        | Some coord -> sprintf "POINT(%s)" (showWktCoord coord)


    /// Prints as 'LINESTRING(-1.08066 53.93863,-1.43627 53.96907)'
    let showWktMultiPoint (source:WktMultiPoint<'a>) : string =
        match unwrapWktMultiPoint source with
        | [] -> "MULTIPOINT EMPTY"
        | xs -> sprintf "MULTIPOINT(%s)" (String.concat "," <| List.map showWktCoord xs)


    /// Prints as 'LINESTRING(-1.08066 53.93863,-1.43627 53.96907)'
    let showWktLineString (source:WktLineString<'a>) : string =
        match unwrapWktLineString source with
        | [] -> "LINESTRING EMPTY"
        | xs -> sprintf "LINESTRING(%s)" (String.concat "," <| List.map showWktCoord xs)


    /// This is a simple closed polygon without any interior polygons.
    /// The user is responsible to ensure the polygon is closed before printing and
    /// that points are in counter-clockwise direction.
    let showWktPolygon (source:WktPolygon<'a>) : string =
        let showRing xs = sprintf "(%s)" << String.concat "," <| List.map showWktCoord xs
        match source.ExteriorRing, source.InteriorRings with
        | [], [] -> "POLYGON EMPTY"
        | xs, xss -> sprintf "POLYGON(%s)" << String.concat "," <| List.map showRing (xs::xss)

        
    // ***** Conversion *****
    let wgs84PointToWKT (point:WGS84Point) : WktPoint<WGS84> =
        WktPoint <| Some { WktLon = decimal point.Longitude; WktLat = decimal point.Latitude }

    let osgb36PointToWKT (point:OSGB36Point) : WktPoint<OSGB36> = 
        WktPoint <| Some { WktLon = decimal point.Easting; WktLat = decimal point.Northing }

    let wktToWGS84Point (point:WktPoint<WGS84>) : WGS84Point option = 
        let change (coord:WktCoord) = { Latitude = 1.0<degree> * float coord.WktLat
                                      ; Longitude = 1.0<degree> * float coord.WktLon }
        Option.map change <| unwrapWktPoint point

    let wktToOSGB36Point (point:WktPoint<OSGB36>) : OSGB36Point option = 
        let change (coord:WktCoord) = { Easting = 1.0<meter> * float coord.WktLon
                                      ; Northing = 1.0<meter> * float coord.WktLat }
        Option.map change <| unwrapWktPoint point


    let wktOSGB36ToWktWGS84 (point:WktPoint<OSGB36>) : WktPoint<WGS84> = 
        match wktToOSGB36Point point with
        | None -> WktPoint None 
        | Some osgb -> wgs84PointToWKT <| osgb36ToWGS84 osgb

    let wktWGS84ToWktOSGB36 (point:WktPoint<WGS84>) : WktPoint<OSGB36> = 
        match wktToWGS84Point point with 
        | None -> WktPoint None
        | Some wgs -> osgb36PointToWKT <| wgs84ToOSGB36 wgs

    // ***** PARSING *****

    let private pSymbol (s:string) : Parser<string,'u> = 
        pstring s .>> spaces

    let private pParens (p:Parser<'a,'u>) : Parser<'a,'u> =
        between (pSymbol "(") (pSymbol ")") p

    // We have got over the value restriction by fixing the WktPoint phantom tag 
    // to TEMP and parser state to unit.
    // This is not a brilliant solution.

    let private pDecimal : Parser<decimal,unit> = pfloat |>> decimal

    type private OneAndMany<'a> = 'a * 'a list

    

    /// This is the pattern for Polygon
    let private sepByOneMany (p:Parser<'a,unit>) (sepa:Parser<'z,unit>) : Parser<OneAndMany<'a>,unit> = 
        parse { let! ans1 = p
                let! ansMany = sepBy p sepa
                return (ans1,ansMany) }

    // This is not within parens
    let private pCoord : Parser<WktCoord, unit> = 
        pipe2   (pDecimal .>> spaces)
                (pDecimal .>> spaces) 
                (fun lon lat -> { WktLon = lon; WktLat = lat })

    let private pEmptySet (emptySet:'a) : Parser<'a,unit> = 
        pSymbol "EMPTY" |>> (fun _ -> emptySet)

    type private PointText = WktCoord option

    let private pPointText : Parser<PointText,unit> = 
        (pSymbol "EMPTY" |>> fun _ -> None) <|> (pParens pCoord |>> Some)



    type private LinestringText = WktCoord list

    let private pLinestringText : Parser<LinestringText, unit> = 
        pEmptySet [] <|> pParens (sepBy1 pCoord (pSymbol ","))
        

    let private pParenLinestringText1 : Parser<LinestringText, unit> = 
        sepBy1 (pParens pCoord) (pSymbol ",")

    type private PolygonText = LinestringText list

    let private pPolygonText : Parser<PolygonText,unit> =
        pEmptySet [] <|> pParens (sepBy1 pLinestringText (pSymbol ","))

    
    type MultipointText = PointText list
    
    // Old...
    let private pWktRing : Parser<WktCoord list, unit> = 
        pParens pLinestringText

    // Old...
    let private pWktRings : Parser<(WktCoord list) list, unit> = 
        sepBy pWktRing (pSymbol ",")


    let private pEMPTY (emptyDefault:'a) : Parser<'a, unit> = 
        pSymbol "EMPTY" |>> (fun _ -> emptyDefault)


    let pGeometry (name:string) (emptyDefault:'a) (p:Parser<'a,unit>) : Parser<'a,unit> = 
        pSymbol name >>. (pEMPTY emptyDefault <|> pParens p)
    
    // Non empty -i.e "EMPTY" token is not an alternative in the data
    let pGeometryNonEmpty (name:string) (p:Parser<'a,unit>) : Parser<'a,unit> = 
        pSymbol name >>. (pParens p)
        
    let tryReadParse (p1:Parser<'a,unit>) (source:string) : 'a option = 
        let ans1 = runParserOnString p1 () "none" source
        match ans1 with
        | Success(a,_,_) -> Some <| a
        | Failure(s,_,_) -> None


    // ***** Parsing Public API

    let tryReadWktPoint (source:string) : WktPoint<'srid> option = 
        let parsePOINT = pSymbol "POINT" >>. (pPointText |>> WktPoint)
        tryReadParse parsePOINT source

    // TODO - the proper version is MULTIPOINT((1 2), (3 4))            
    let tryReadWktMultiPoint (source:string) : WktMultiPoint<'srid> option = 
        let parseMULTIPOINT : Parser<WktMultiPoint<'srid>, unit>= 
            pGeometry "MULTIPOINT" [] (pParenLinestringText1 <|> pLinestringText) |>> WktMultiPoint
        tryReadParse parseMULTIPOINT source

    let tryReadWktLineString (source:string) : WktLineString<'srid> option = 
        let parseLINESTRING = pSymbol "LINESTRING" >>. pLinestringText |>> WktLineString
        tryReadParse parseLINESTRING source

    let private buildPolygon (source:PolygonText) : WktPolygon<'srid> = 
        match source with
        | [] -> { ExteriorRing = []; InteriorRings = [] }
        | x :: xs -> { ExteriorRing = x; InteriorRings = xs }



    // WARNING - to test
    let tryReadWktPolygon1 (source:string) : WktPolygon<'srid> option = 
        let parsePOLYGON1 = pSymbol "POLYGON" >>. pPolygonText |>> buildPolygon
        tryReadParse parsePOLYGON1 source




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

    // Note for the API - isClosed (predicate) may be more important 
    // than closePolygon (action).
    // Can we expect data from client codeto be always closed?
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



