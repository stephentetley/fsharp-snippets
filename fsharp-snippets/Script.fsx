﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Coord.fs"
#load "GeoDistance.fs"

open Coord
open GeoDistance
open System

let testZ1 = Coord.ns 3 (Seq.toList "352263")
let testZ2 = Char.GetNumericValue '6'

let phi1 = Coord.fromDMS 52 39 27.2531
let lam1 = Coord.fromDMS 1 43 4.5177

let test01 = Coord.latlonToEN {Latitude = phi1; Longitude = lam1}
let test02 = Coord.enToLatLon {Eastings = 651409.903; Northings = 313177.270}

let test03 = Coord.decodeMinor 'c' ;;
let testit c = Coord.decodeAlpha c

// e.g. > TestScript.test01;;

// Should not compile...
// let temp = Coord.N0;; 

/// SW - Easting : 135247    Northing : 026365
let cove_hill = Coord.fromOSGridRef10 "SW3524726365"

/// NE - Easting :  335676   Northing : 970738
let wick = Coord.fromOSGridRef10 "ND3567670738"

/// NW - Easting : 106697    Northing : 937542
let isle_of_lewis = Coord.fromOSGridRef10 "NB0669737542"

/// SE - Easting : 628458    Northing : 141168
let dover = Coord.fromOSGridRef10 "TR2845841168"

let dover2 = {Coord.Eastings = 628458.0; Coord.Northings =  141168.0}

#load "GeoDistance.fs"
open GeoDistance

let testD1 = match dover with
             | Some d1 -> GeoDistance.haversineDistance (Coord.enToLatLon d1) (Coord.enToLatLon dover2)
             | None -> failwith "Bad"


