open System

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#load @"SL\Tolerance.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
open SL.Geo.Coord
open SL.Geo.WellKnownText


let testZ2 = Char.GetNumericValue '6'

let phi1 : float<degree> = makeDegree 52 39 27.2531
let lam1 : float<degree> = makeDegree 1 43 4.5177

let test01 = wgs84ToOSGB36 {Latitude = phi1; Longitude = lam1}
let test02 = osgb36ToWGS84 {Easting = 651409.903<meter>; Northing = 313177.270<meter>}

//let test03 = decodeMinor 'c' ;;
//let testit c = decodeAlpha c

// e.g. > TestScript.test01;;

// Should not compile...
// let temp = N0;; 

/// SW - Easting : 135247    Northing : 026365
let coveHill = 
    readOSGB36Point "SW3524726365"

/// NE - Easting :  335676   Northing : 970738
let wick = 
    readOSGB36Point "ND3567670738"

/// NW - Easting : 106697    Northing : 937542
let isleOfLewis = 
    readOSGB36Point "NB0669737542"

/// SE - Easting : 628458    Northing : 141168
let dover : OSGB36Point = 
    readOSGB36Point "TR2845841168"

let dover2 = {Easting = 628458.0<meter>; Northing =  141168.0<meter>}


let testD1 () = 
    haversineDistanceOGSB36Point dover dover2


let read01 () = 
    readOSGB36Point "ND3567670738"

let read02 () = 
    readOSGB36Point "ND35676 70738"

let read03 () = 
    readOSGB36Point "ND 3567 7073"


let wktRead01 () = 
    printfn "%A" <| tryReadWktPoint "POINT(-1.98073 53.72879)"

let wktRead02 () =
    printfn "%A" <| tryReadWktLineString "LINESTRING(-1.98073 53.72879, -1.90546 53.70771, -1.85249 53.72091, -1.75227 53.74991, -1.74833 53.79111)" 

let wktRead03 () =
    printfn "%A" <| tryReadWktMultiPoint "MULTIPOINT(-1.98073 53.72879, -1.90546 53.70771, -1.85249 53.72091, -1.75227 53.74991, -1.74833 53.79111)" 

let wktRead04 () =  
    let (polygon1:WktPolygon<WGS84> option) = 
        tryReadWktPolygon1 "POLYGON(-1.98073 53.72879, -1.90546 53.70771, -1.85249 53.72091, -1.75227 53.74991, -1.74833 53.79111)" 
    printfn "%A" polygon1


