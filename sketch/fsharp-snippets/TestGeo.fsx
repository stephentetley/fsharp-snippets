#load "Coord.fs"
#load "GeoDistance.fs"

open Coord
open GeoDistance
open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

let testZ1 = Coord.ns 3 (Seq.toList "352263")
let testZ2 = Char.GetNumericValue '6'

let phi1 : float<Coord.degree> = Coord.fromDMS 52 39 27.2531
let lam1 : float<Coord.degree> = Coord.fromDMS 1 43 4.5177

let test01 = Coord.latlonToEN {Latitude = phi1; Longitude = lam1}
let test02 = Coord.enToLatLon {Eastings = 651409.903<meter>; Northings = 313177.270<meter>}

let test03 = Coord.decodeMinor 'c' ;;
let testit c = Coord.decodeAlpha c

// e.g. > TestScript.test01;;

// Should not compile...
// let temp = Coord.N0;; 

/// SW - Easting : 135247    Northing : 026365
let coveHill = Coord.fromOSGridRef10 "SW3524726365"

/// NE - Easting :  335676   Northing : 970738
let wick = Coord.fromOSGridRef10 "ND3567670738"

/// NW - Easting : 106697    Northing : 937542
let isleOfLewis = Coord.fromOSGridRef10 "NB0669737542"

/// SE - Easting : 628458    Northing : 141168
let dover = Coord.fromOSGridRef10 "TR2845841168"

let dover2 = {Coord.Eastings = 628458.0<meter>; Coord.Northings =  141168.0<meter>}


let testD1 = match dover with
             | Some d1 -> GeoDistance.haversineDistance (Coord.enToLatLon d1) (Coord.enToLatLon dover2)
             | None -> failwith "Bad"


