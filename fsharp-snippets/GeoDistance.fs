namespace GeoDistance

open Coord
open System

module GeoDistance = 

    type kilometres = double

    // let temp = Coord.WGS84Point 0 0 

    // Operates on WGS84Points i.e. Lat-Lon
    let haversineDistance (p1 : Coord.WGS84Point) (p2 : Coord.WGS84Point) = 
        let radius = (6371.000 : kilometres)
        let lat1R = Coord.deg2rad(p1.Latitude)
        let lat2R = Coord.deg2rad(p2.Latitude)
        let latDeltaR = Coord.deg2rad(p2.Latitude-p1.Latitude)
        let lonDeltaR = Coord.deg2rad(p2.Longitude-p1.Longitude);
        let a = Math.Sin(latDeltaR /2.0) * Math.Sin(latDeltaR /2.0) + 
                Math.Cos(lat1R) * Math.Cos(lat2R) *
                Math.Sin(lonDeltaR / 2.0) * Math.Sin(lonDeltaR/2.0)
        
        let c = 2.0 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.0-a))

        let ans= radius * c
        ans


