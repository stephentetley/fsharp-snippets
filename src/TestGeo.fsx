﻿open System

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#load @"SL\Tolerance.fs"
#load @"SL\Coord.fs"
#load @"SL\WellKnownText.fs"
open SL.Geo


let testZ2 = Char.GetNumericValue '6'

let phi1 : float<Coord.degree> = Coord.makeDegree 52 39 27.2531
let lam1 : float<Coord.degree> = Coord.makeDegree 1 43 4.5177

let test01 = Coord.wgs84ToOSGB36Point {Latitude = phi1; Longitude = lam1}
let test02 = Coord.osgb36PointToWGS84 {Easting = 651409.903<meter>; Northing = 313177.270<meter>}

//let test03 = Coord.decodeMinor 'c' ;;
//let testit c = Coord.decodeAlpha c

// e.g. > TestScript.test01;;

// Should not compile...
// let temp = Coord.N0;; 

/// SW - Easting : 135247    Northing : 026365
let coveHill = 
    Coord.readOSGB36Point "SW3524726365"

/// NE - Easting :  335676   Northing : 970738
let wick = 
    Coord.readOSGB36Point "ND3567670738"

/// NW - Easting : 106697    Northing : 937542
let isleOfLewis = 
    Coord.readOSGB36Point "NB0669737542"

/// SE - Easting : 628458    Northing : 141168
let dover : Coord.OSGB36Point = 
    Coord.readOSGB36Point "TR2845841168"

let dover2 = {Coord.Easting = 628458.0<meter>; Coord.Northing =  141168.0<meter>}


let testD1 () = 
    Coord.haversineDistanceOGSB36Point dover dover2


let read01 () = 
    Coord.readOSGB36Point "ND3567670738"

let read02 () = 
    Coord.readOSGB36Point "ND35676 70738"

let read03 () = 
    Coord.readOSGB36Point "ND 3567 7073"