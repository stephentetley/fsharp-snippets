namespace SL.Geo

open System
open System.Text.RegularExpressions

open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open SL.Geo.Coord
open SL.Geo.WellKnownText

// This is the coordinate system of OpenStreetMap data

module WebMercator = 
    

    [<StructuredFormatDisplay("{WmEasting}E {WmNorthing}N:3857")>]
    type WMPoint = 
        { WmEasting : float<meter>
          WmNorthing : float<meter> }

    /// Projection used by Open Street Map and Google Maps
    /// There are several SRID for this system is ESPG:3857, ESPG:900913, ...
    type WebMercator = class end

    // ***** construction / Conversion *****
    let wmWktCoord (point:WMPoint) : WktCoord =
        { WktLon = decimal point.WmEasting; WktLat = decimal point.WmNorthing }

    let wktCoordToWM (coord:WktCoord) : WMPoint =
        { WmEasting = 1.0<meter> * float coord.WktLon
        ; WmNorthing = 1.0<meter> * float coord.WktLat }