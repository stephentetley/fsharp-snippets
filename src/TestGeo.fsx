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

// TODO - this is invalid according to the later revision of the spec, but we should still be able
// to parse it
let wktRead03 () =
    printfn "%A" <| tryReadWktMultiPoint "MULTIPOINT(-1.98073 53.72879, -1.90546 53.70771, -1.85249 53.72091, -1.75227 53.74991, -1.74833 53.79111)" 

let wktRead03a () =
    printfn "%A" <| tryReadWktMultiPoint "MULTIPOINT((-1.98073 53.72879), (-1.90546 53.70771), (-1.85249 53.72091), (-1.75227 53.74991), (-1.74833 53.79111))" 



let wktRead04 () =  
    let (polygon1:WktPolygon<WGS84> option) = 
        tryReadWktPolygon "POLYGON((-0.576 53.724,-0.578 53.722,-0.580 53.722,-0.622 53.748, -0.4594 54.007,-0.370 54.039,-0.312 54.046,0.120 53.650,0.118 53.648,0.076 53.650,-0.392 53.824,-0.453 53.789,-0.455 53.789,-0.498 53.789,-0.572 53.732,-0.576 53.724))"
    printfn "%A" polygon1


let wktRead05 () =  
    let (triangle1:WktTriangle<WGS84> option) = 
        tryReadWktTriangle "TRIANGLE((-0.576 53.724,-0.578 53.722,-0.580 53.722))"
    printfn "%A" triangle1

let dist01 () =
    // ST_Distance(ST_GeogFromText('SRID=4326;POINT (-0.90565 54.46504)'),ST_GeogFromText('SRID=4326;POINT (-0.91998 54.46242)')));
    haversineDistance   {Longitude = -0.90565<degree>; Latitude=54.46504<degree>}
                        {Longitude = -0.91998<degree>; Latitude=54.46242<degree>}   


let dist02 () =
    haversineDistanceOGSB36Point    {Easting=422494.690<meter>; Northing=505852.776<meter>}  
                                    {Easting=410700.754<meter>; Northing=510164.568<meter>} 
