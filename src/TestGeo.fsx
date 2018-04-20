#I @"..\packages\FParsec.1.0.3\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\Tolerance.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
#load @"SL\WebMercator.fs"
open SL.Geo.Coord
open SL.Geo.WellKnownText
open SL.Geo.WebMercator

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

/// Off topic KML is EPSG:4326.


/// Work towards WebMercator <-> WGS84
let osgb36BaildonHillTP = {Easting=414124.69<meter>; Northing=440072.41<meter> }

let wgs84BaildonHillTP = { Longitude = -1.7867456<degree>; Latitude = 53.856689<degree> }

let wmBaildonHill = { WmEasting = -198899.61036912<meter>; WmNorthing = 7143061.49042226<meter> }


let arctanh (x:float) : float= (log(1.0+x) - log(1.0-x))/2.0

let wmEasting (lon:float<degree>) : float = 
    let a = 6378137.0
    let lam = degreeToRadian lon
    a * float lam

let wmNorthing (lat:float<degree>) : float = 
    let a = 6378137.0
    let phi = degreeToRadian lat
    a * arctanh (sin <| float phi)

let wmTest01 () = 
    wgs84ToWM wgs84BaildonHillTP

let xWGS (xwm:float<meter>) : float<degree> = 
    let aWGS = 6378137.0
    radianToDegree <| 1.0<radian> * (float xwm) / aWGS

let yWGS (ywm:float<meter>) : float<degree> = 
    let aWGS = 6378137.0
    radianToDegree <| 1.0<radian> * (Math.Atan(Math.Exp (float ywm / aWGS)) * 2.0 - (Math.PI / 2.0))

let temp100 () = 
    let osgb1 = wgs84ToOSGB36 {Latitude = 53.6060746<degree>; Longitude = -1.019215849<degree>}
    showOSGB36Point <| osgb1

    // should be "SE 64994 12615"

// 464994E 412615N

let temp101 () = 
    showOSGB36Point <| {Easting = 464994.0<meter>; Northing = 412615.0<meter>}


let ed50: HelmertParams = 
    { DX = 89.5; DY = 93.8; DZ = 123.1; RotX = 0.0; RotY = 0.0; RotZ = 0.156; Scale = -1.200 }
    
let osgb: HelmertParams = 
    { DX = -446.448; DY = 125.157; DZ = -542.060; RotX = -0.150; RotY = -0.247; RotZ = -0.842; Scale = 20.4894 }

let temp102 () = 
    let phi = makeDegree 53 0 0.0
    let lam = makeDegree 1 0 0.0
    let cc1 = toCC3D airy1830 phi lam 50.00<meter>;
    let cc2 = helmertTransform osgb cc1
    let ans = fromCC3D airy1830 cc2
    (ans, cc1, cc2)

let temp102a () = 
    let phi = makeDegree 53 0 0.0
    let lam = makeDegree 1 0 0.0
    let cc1 = toCC3D airy1830 phi lam 50.00<meter>;
    let ans = fromCC3D airy1830 cc1
    (ans, cc1)

let temp103 () = 
    toCC3D airy1830 phi1 lam1 24.700<meter>

// showOSGB36Point is fine, wgs84ToOSGB36 is inaccurate

// let deg2rad (d : float) : float = (Math.PI/180.0) * d

// let rad2deg (r : float) : float = (180.0/Math.PI) * r
