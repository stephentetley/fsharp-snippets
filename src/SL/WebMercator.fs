namespace SL.Geo

open System
open System.Text.RegularExpressions

open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open SL.Geo.Coord
open SL.Geo.WellKnownText

// This is the coordinate system of OpenStreetMap data

module WebMercator = 
    

    [<StructuredFormatDisplay("{WmEasting}E {WmNorthing}N:7483")>]
    type WMPoint = 
        { WmEasting : float<meter>
          WmNorthing : float<meter> }

    let private arctanh (x:float) : float= (log(1.0+x) - log(1.0-x))/2.0

    let wgs84ToWM ({Latitude = lat; Longitude = lon} : WGS84Point) : WMPoint = 
        let a = 6378137.0
        let lam = degreeToRadian lon
        let phi = degreeToRadian lat
        { WmEasting = 1.0<meter> * a * float lam
        ; WmNorthing= 1.0<meter> * a * arctanh (sin <| float phi) }

    /// Projection used by Open Street Map and Google Maps
    /// There are several SRID for this system is ESPG:3857, ESPG:900913, ...
    type WebMercator = class end

    // ***** construction / Conversion *****
    let wktIsoWebMercator:WktCoordIso<WMPoint,WebMercator> = 
        { SRID = 7483
        ; Spheroid = "SPHEROID[\"WGS 84\",6378137,298.257223563]"
        ; ToWktCoord = 
            fun point -> 
                { WktLon = decimal point.WmEasting
                ; WktLat = decimal point.WmNorthing }
        ; FromWktCoord = 
            fun coord -> 
                { WmEasting = 1.0<meter> * float coord.WktLon
                ; WmNorthing = 1.0<meter> * float coord.WktLat }
        }


    let wmWktCoord (point:WMPoint) : WktCoord =
        { WktLon = decimal point.WmEasting; WktLat = decimal point.WmNorthing }

    let wktCoordToWM (coord:WktCoord) : WMPoint =
        { WmEasting = 1.0<meter> * float coord.WktLon
        ; WmNorthing = 1.0<meter> * float coord.WktLat }