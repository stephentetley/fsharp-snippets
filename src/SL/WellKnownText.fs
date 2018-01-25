namespace SL.Geo

open System
open System.Text.RegularExpressions

//open Microsoft.FSharp.Core
//open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open FParsec
//open FSharpx.Collections


open SL.Tolerance
open SL.Geo.Coord



module WellKnownText = 
    
    // Note - Wkt should not favour WGS84 srids.
    // Other spatial references with Lon & Lat are possible.

    /// Encode coordinate reference system as a phantom type.
    /// Values are represented as decimal
    type WktPoint<'a> = 
        { WktLon: decimal      
          WktLat: decimal }

    
    /// World Geodetic System 1984             
    /// The SRID for this system is ESPG:4326
    type WGS84 = class end

    /// Ordinance Survey Great Britain National Grid reference system 
    /// The SRID for this system is ESPG:27700
    type OSGB36 = class end

    let wktPointsEqual (tx:Tolerance) (p1:WktPoint<'a>) (p2:WktPoint<'a>) : bool =
        tEqual tx p1.WktLon p2.WktLon && tEqual tx p1.WktLat p2.WktLat

    // TODO - wrap with a constructor or just an alias?
    type WktLineString<'a> = WktPoint<'a> list 
    
    type WktPolygon<'a> = WktPoint<'a> list 

    /// Prints as 'POINT(14.12345, 15.12345)'
    let inline showWktPoint (point:WktPoint<'a>) : string = 
        sprintf "POINT(%.5f %.5f)" point.WktLon point.WktLat

    let inline private showWktPoint1 (point:WktPoint<'a>) : string = 
        sprintf "%.5f %.5f" point.WktLon point.WktLat

    /// Prints as 'LINESTRING(-1.08066 53.93863,-1.43627 53.96907)'
    let showWktLineString (points:WktLineString<'a>) : string =
        match points with
        | [] -> "LINESTRING EMPTY"
        | _ -> sprintf "LINESTRING(%s)" (String.concat "," <| List.map showWktPoint1 points)


    /// This is a simple closed polygon without any interior polygons.
    /// The user is responsible to ensure the polygon is closed before printing and
    /// that points are in counter-clockwise direction.
    let showWktPolygon (points:WktPolygon<'a>) : string =
        match points with
        | [] -> "POLYGON EMPTY"
        | _ -> sprintf "POLYGON(%s)" (String.concat "," <| List.map showWktPoint1 points)  


    let wgs84PointAsWKT (point:WGS84Point) : WktPoint<WGS84> = 
        { WktLon = decimal point.Longitude     
          WktLat = decimal point.Latitude }

    let osgb36PointAsWKT (point:OSGB36Point) : WktPoint<OSGB36> = 
        { WktLon = decimal point.Easting    
          WktLat = decimal point.Northing }

  

    // ***** PARSING *****

    let private pSymbol (s:string) : Parser<string,'u> = 
        pstring s .>> spaces

    let private pParens (p:Parser<'a,'u>) : Parser<'a,'u> =
        between (pSymbol "(") (pSymbol ")") p

    // TODO - check FParsec source to see how spaces overcomes the value restriction
    let pDecimal : Parser<decimal,unit> = pfloat |>> (fun a -> decimal a)

    let pWktPoint1 () : Parser<WktPoint<'a>, unit> = 
        pipe2 pDecimal pDecimal (fun lon lat -> { WktLon = lon; WktLat = lat })

    // Utility combinators
    let private parsePOINT () : Parser<WktPoint<'a>, unit> = 
        pSymbol "POINT" >>. pParens (pWktPoint1 ())

    let tryReadWktPoint (source:string) : WktPoint<'a> option = 
        let ans1 = runParserOnString (parsePOINT ()) () "none" source
        match ans1 with
        | Success(a,_,_) -> Some a
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

