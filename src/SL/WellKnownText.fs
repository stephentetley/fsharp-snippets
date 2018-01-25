namespace SL.Geo

open System
open System.Text.RegularExpressions

open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open SL.Tolerance
open SL.Geo.Coord



module WellKnownText = 
    
    // Note - Wtk should not favour WGS84 srids.
    // Other spatial references with Lon & Lat are possible.

    /// Encode SRID as a phantom type.
    /// Values are represented as decimal
    type WtkPoint<'a> = 
        { WtkLon: decimal      
          WtkLat: decimal }

    type WGS84 = class end
    type OSGB36 = class end

    let wtkPointsEqual (tx:Tolerance) (p1:WtkPoint<'a>) (p2:WtkPoint<'a>) : bool =
        tEqual tx p1.WtkLon p2.WtkLon && tEqual tx p1.WtkLat p2.WtkLat

    // TODO - wrap with a constructor or just an alias?
    type WtkLineString<'a> = WtkPoint<'a> list 
    
    type WtkPolygon<'a> = WtkPoint<'a> list 

    /// Prints as 'POINT(14.12345, 15.12345)'
    let inline showWtkPoint (point:WtkPoint<'a>) : string = 
        sprintf "POINT(%.5f %.5f)" point.WtkLon point.WtkLat

    let inline private showWtkPoint1 (point:WtkPoint<'a>) : string = 
        sprintf "%.5f %.5f" point.WtkLon point.WtkLat

    /// Prints as 'LINESTRING(-1.08066 53.93863,-1.43627 53.96907)'
    let showWtkLineString (points:WtkLineString<'a>) : string =
        match points with
        | [] -> "LINESTRING EMPTY"
        | _ -> sprintf "LINESTRING(%s)" (String.concat "," <| List.map showWtkPoint1 points)


    /// This is a simple closed polygon without any interior polygons.
    /// The user is responsible to ensure the polygon is closed before printing and
    /// that points are in counter-clockwise direction.
    let showWtkPolygon (points:WtkPolygon<'a>) : string =
        match points with
        | [] -> "POLYGON EMPTY"
        | _ -> sprintf "POLYGON(%s)" (String.concat "," <| List.map showWtkPoint1 points)  


    let wgs84PointAsWKT (point:WGS84Point) : WtkPoint<WGS84> = 
        { WtkLon = decimal point.Longitude     
          WtkLat = decimal point.Latitude }

    let osgb36PointAsWKT (point:OSGB36Point) : WtkPoint<OSGB36> = 
        { WtkLon = decimal point.Easting    
          WtkLat = decimal point.Northing }

  
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

