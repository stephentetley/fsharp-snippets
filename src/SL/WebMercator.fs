namespace SL.Geo

open System
open System.Text.RegularExpressions

open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

open SL.Geo.Coord

// This is the coordinate system of OpenStreetMap data

module WebMercator = 
    

    [<StructuredFormatDisplay("{WmEasting}E {WmNorthing}N:3857")>]
    type WMPoint = 
        { WmEasting : float<meter>
          WmNorthing : float<meter> }