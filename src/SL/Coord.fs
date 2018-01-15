namespace SL.Geo

open System
open System.Text.RegularExpressions

open Microsoft.FSharp.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

module Coord = 

    // kilometer and degree can probably go into their own module...

    // Units of measure make the internals very complicated (perhaps limit to the external interface?)
    [<Measure>]
    type kilometer

    // type metres = double    
    
    [<Measure>]
    type degree


    // Note - the the grid letter plus grid digits is a synonymous representation for OSGB36
    // E.g Sullom Voe oil terminal in the Shetlands can be specified as HU396753 or 439668,1175316.
    // So we have two versions.

    [<StructuredFormatDisplay("{Eastings}E {Northings}E")>]
    type OSGB36Point = { Eastings : float<meter>; Northings : float<meter> }

    [<StructuredFormatDisplay("{Letter1}{Letter2} {Eastings} {Northings}")>]
    type OSGB36Grid = 
        { Letter1 : char; 
          Letter2 : char; 
          Eastings : float<meter>; 
          Northings : float<meter> }

    [<StructuredFormatDisplay("{Latitude}Lat {Longitude}Lon")>]
    type WGS84Point = { Latitude : float<degree>; Longitude : float<degree> }

    let inline private deg2rad (d : float) = (Math.PI/180.0) * d

    let inline private rad2deg (r : float) = (180.0/Math.PI) * r

    /// fromDMS :: Int -> Int -> Double -> DDegrees
    let makeDegree (d : int) (m : int) (s : float) : float<degree> = 
        LanguagePrimitives.FloatWithMeasure (float d + float m / 60.0 + s / 3600.0)

    // ellipsoid constants for Airy 1830
    let (private a : float) = 6377563.396

    let (private b : float) = 6356256.909

    // National Grid coordinates of true origin (E0, N0) for Airy 1830 (National Grid)

    let (private E0 : float) = 400000.0

    let (private N0 : float) = -100000.0

    // Scale Factor on Central Meridian (F0) for Airy 1830 (National Grid)


    let (private F0 : float) = 0.9996012717

    // True origin (phi0, lam0)
    let private phi0 = deg2rad 49.0


    // 2deg West is (-2)
    let private lam0 = deg2rad -2.0

    // Eccentricity
    let (private e2 : float) = ((a*a) - (b*b)) / (a*a)

    // Equation C1 to get n
    
    let private n = (a-b) / (a+b)

    let private n2 = n*n

    let private n3 = n*n*n

    let private equationC2 (phi : double) = 
        let sin2Phi = sin phi * sin phi
        let nu = a * F0 * (1.0 - e2 * sin2Phi) ** -0.5
        let rho = a * F0 * (1.0 - e2) * (1.0 - e2 * sin2Phi) ** -1.5
        let eta2 = nu / rho - 1.0
        (nu, rho, eta2)
    
    let private equationC3 (phi : double) =  
        let phiPphi0  = phi + phi0
        let phiMphi0  = phi - phi0
        let Ma = (1.0 + n + (5.0/4.0)*n2 + (5.0/4.0)*n3) * phiMphi0
        let Mb = (3.0*n + 3.0*n2 + (21.0/8.0)*n3) * sin phiMphi0 * cos phiPphi0
        let Mc = ((15.0/8.0)*n2 + (15.0/8.0)*n3) * sin (2.0*phiMphi0) * cos (2.0*(phiPphi0))
        let Md = ((35.0/24.0)*n3) * sin (3.0*phiMphi0) * cos (3.0*phiPphi0)
        b * F0 * (Ma - Mb + Mc - Md)

    let wgs84ToOSGB36Point ({Latitude = phidd; Longitude = lamdd} : WGS84Point) : OSGB36Point = 
        let phi = deg2rad (float phidd)
        let lam = deg2rad (float lamdd)
        let sinPhi = sin phi
        let cosPhi = cos phi
        let tanPhi = tan phi
        let tan2Phi = tanPhi * tanPhi
        let cos3Phi = cosPhi * cosPhi * cosPhi
        let tan4Phi = tanPhi * tanPhi * tanPhi * tanPhi
        let cos5Phi = cosPhi * cosPhi * cosPhi * cosPhi * cosPhi
        let lamMlam0 = lam - lam0
        let (nu,rho,eta2) = equationC2 phi 

        let M = equationC3 phi
        let I = M + N0
        let II = nu / 2.0 * sinPhi * cosPhi
        let III = nu / 24.0 * sinPhi * cos3Phi * (5.0 - tan2Phi + 9.0 * eta2)
        let IIIA = nu / 720.0 * sinPhi * cos5Phi * (61.0 - 58.0 * tan2Phi + tan4Phi)
        let IV = nu * cosPhi
        let V = nu / 6.0 * cos3Phi * (nu / rho - tan2Phi)
        let VI = nu / 120.0 * cos5Phi * (5.0 - 18.0 * tan2Phi + tan4Phi + 14.0 * eta2 - 58.0 * tan2Phi * eta2)
        let N = I + II * lamMlam0 ** 2.0 + III * lamMlam0 ** 4.0 + IIIA * lamMlam0 ** 6.0
        let E = E0 + IV * lamMlam0 + V * lamMlam0 ** 3.0 + VI * lamMlam0 ** 5.0
        { Eastings = E * 1.0<meter>; Northings = N * 1.0<meter> }


    let osgb36PointToWGS84 (osgb36:OSGB36Point) : WGS84Point =
        let E               = osgb36.Eastings
        let N               = osgb36.Northings
        let rec makePhi p m = 
            if abs (float N - N0 - m) < 0.01 then 
                p 
            else
                let phiNEW = (float N - N0 - m) / (a * F0) + p
                let Mnew   = equationC3 phiNEW
                makePhi phiNEW Mnew
        let phi1'           = (float N - N0) / (a * F0) + phi0
        let M1              = equationC3 phi1'
        let phi'            = makePhi phi1' M1
        let (nu,rho,eta2)   = equationC2 phi'
        let tanPhi'         = tan phi'
        let cosPhi'         = cos phi'
        let secPhi'         = 1.0 / cosPhi'
        let tan2Phi'        = tanPhi' * tanPhi'
        let tan4Phi'        = tanPhi' * tanPhi' * tanPhi' * tanPhi'
        let tan6Phi'        = tanPhi' * tanPhi' * tanPhi' * tanPhi' * tanPhi' * tanPhi'
        let Em_E0          = float E - E0

        let VII        = tanPhi' / (2.0 * rho * nu)
        let VIII       = tanPhi' / (24.0 * rho * nu ** 3.0) * (5.0 + 3.0 * tan2Phi' + eta2 - 9.0 * tan2Phi' * eta2)
        let IX         = tanPhi' / (720.0 * rho * nu ** 5.0) * (61.0 + 90.0 * tan2Phi' + 45.0 * tan4Phi')
        let X          = secPhi' / nu
        let XI         = secPhi' / (6.0 * nu ** 3.0) * (nu / rho + 2.0 * tan2Phi')
        let XII        = secPhi' / (120.0 * nu ** 5.0) * (5.0 + 28.0 * tan2Phi' + 24.0 * tan4Phi')
        let XIIA       = secPhi' / (5040.0 * nu ** 7.0) * (61.0 + 662.0 * tan2Phi' + 1320.0 * tan4Phi' + 720.0 * tan6Phi')
        let phi         = phi' - VII * (Em_E0 ** 2.0) + VIII * (Em_E0 ** 4.0) - IX * (Em_E0 ** 6.0)
        let lam         = lam0 + X * Em_E0 - XI * (Em_E0 ** 3.0) + XII * (Em_E0 ** 5.0) - XIIA * (Em_E0 ** 7.0)
        { Latitude = LanguagePrimitives.FloatWithMeasure (rad2deg phi)
        ; Longitude = LanguagePrimitives.FloatWithMeasure (rad2deg lam) } 


  

    /// OS Grid refs, see
    /// https://en.wikipedia.org/wiki/Ordnance_Survey_National_Grid

   
    let private decodeMajor (ch : char) : (float*float) = 
        match ch with
            | 'S' | 's' -> (0.0, 0.0)
            | 'T' | 't' -> (500000.0, 0.0)
            | 'N' | 'n' -> (0.0, 500000.0)
            | 'O' | 'o' -> (500000.0, 500000.0)
            | 'H' | 'h' -> (0.0, 1000000.0)
            | _         -> (-1000000.0, -1000000.0)

    let private decodeMinor (ch : char) : (float*float) =  
        let shifti x  = 
            match x with
                // There is no 'i' in the list of grid letters
                | _ when x > 8  -> x-1
                | _             -> x

        let fn =  shifti << (fun i -> i - 65) << int << Char.ToUpper
        let (n0, e1) = Math.DivRem(fn ch,5)
        let n1 = 4 - n0
        (float e1 * 100000.0, float n1 * 100000.0)

    let private decodeAlpha (s : char) (t : char) : (float*float) =  
        let (eM, nM) = decodeMajor s
        let (em, nm) = decodeMinor t
        (eM + em, nM + nm)

    // Expects even length string
    let private readContigNumberPair (ss : string) : int*int = 
        if (String.length ss) % 2 = 0 then
            let sz = String.length ss / 2
            let left = ss.[0..sz-1]
            let right = ss.[sz..(sz*2-1)]
            (int left,int right)
        else
            (0,0)


    let private findMajor (easting:float) (northing:float) : char =
        match (easting,northing) with
        | _ when easting >= 0.0 && easting < 500000.0 && northing >= 0.0 && northing < 500000.0 -> 'S'
        | _ when easting >= 500000.0 && easting < 1000000.0 && northing >= 0.0 && northing < 500000.0 -> 'T'
        | _ when easting >= 0.0 && easting < 500000.0 && northing >= 500000.0 && northing < 1000000.0 -> 'N'
        | _ when easting >= 500000.0 && easting < 1000000.0 && northing >= 500000.0 && northing < 1000000.0 -> 'O'
        | _ when easting >= 0.0 && easting < 500000.0 && northing >= 1000000.0 && northing < 1500000.0 -> 'H'
        | _ when easting >= 500000.0 && easting < 1000000.0 && northing >= 1000000.0 && northing < 1500000.0 -> 'J'
        | _ -> 'X'

    let private minorGrid : char[,] = 
        array2D [   [ 'V'; 'Q'; 'L'; 'F'; 'A' ];
                    [ 'W'; 'R'; 'M'; 'G'; 'B' ];
                    [ 'X'; 'S'; 'N'; 'H'; 'C' ];
                    [ 'Y'; 'T'; 'O'; 'J'; 'D' ];
                    [ 'Z'; 'U'; 'P'; 'K'; 'E' ]     ]

    let private findMinor (easting:float) (northing:float) : char =
        let modE = easting % 500000.0
        let modN = northing % 500000.0
        let divE = int (modE / 100000.0)
        let divN = int <| modN / 100000.0
        if divE >=0 && divE < 5 && divN >= 0 && divN < 5 then
            minorGrid.[divE,divN]
        else 'X'

    let private makeOSGB36Grid (c1:char) (c2:char) (east:int) (north:int) : OSGB36Grid =  
        { Letter1 = c1; Letter2 = c2; Eastings = 1.0<meter> *float east; Northings = 1.0<meter> * float north  }

        
    let private makeOSGB36Point (m : char) (mm : char) (east : float<meter>) (north : float<meter>) : OSGB36Point = 
        let (majE, majN) = decodeAlpha m mm 
        { Eastings = east + LanguagePrimitives.FloatWithMeasure majE
        ; Northings = north + LanguagePrimitives.FloatWithMeasure majN }
    

    let osgb36PointToGrid ({Eastings = easting; Northings = northing} : OSGB36Point) : OSGB36Grid =  
        let major = findMajor (float easting) (float northing)
        let minor = findMinor (float easting) (float northing)
        let smallE = easting % 100000.0<meter>
        let smallN = northing % 100000.0<meter>
        { Letter1 = major; Letter2 = minor; Eastings = smallE; Northings = smallN }
    
    let osgb36GridToPoint (gridRef:OSGB36Grid) : OSGB36Point =
        makeOSGB36Point gridRef.Letter1 gridRef.Letter2 gridRef.Eastings gridRef.Northings


    let osgb36GridToWGS84 (gridref : OSGB36Grid) : WGS84Point =
        osgb36PointToWGS84 <| osgb36GridToPoint gridref

    let wgs84ToOSGB36Grid (latlon : WGS84Point) : OSGB36Grid = 
        osgb36PointToGrid <| wgs84ToOSGB36Point latlon

    // Needs 0 padding, width five
    let showOSGB36Grid (pt:OSGB36Grid) : string = 
        sprintf "%c%c%05i%05i" pt.Letter1 pt.Letter2 (int pt.Eastings) (int pt.Northings)

    let private (|OSGB36Regex|_|) (pattern:string) (input:string) : option<GroupCollection> =
        let m = Regex.Match(input.Trim(), pattern)
        if m.Success then Some m.Groups
        else None
    
    let private decodeOSGBNumber1 (s:string) : int =
        match s.Length with
        | 1 -> 10000 * System.Convert.ToInt32 s
        | 2 -> 1000 * System.Convert.ToInt32 s
        | 3 -> 100 * System.Convert.ToInt32 s
        | 4 -> 10 * System.Convert.ToInt32 s
        | 5 -> System.Convert.ToInt32 s
        | _ -> 0

    // precondition length = 6, 8 or 10
    let private decodeOSGBNumber2 (ss:string) : int*int = 
        let width = String.length ss
        let (a,b) = readContigNumberPair ss
        match width with 
        | 6 -> (a*100, b*100)
        | 8 -> (a*10, b*10)
        | 10 -> (a,b)
        | _ -> (0,0)

    let tryReadOSGB36Grid (input:string) : OSGB36Grid option = 
        let getChar1 (groups:GroupCollection) = groups.[1].Value.[0]
        let getChar2 (groups:GroupCollection) = groups.[2].Value.[0]
        match input with
        | OSGB36Regex @"^([A-Za-z])([A-Za-z])\s*([0-9]+)$" groups -> 
            let (e,n) = decodeOSGBNumber2 (groups.[3].Value)
            Some <| makeOSGB36Grid (getChar1 groups) (getChar2 groups) e n
        | OSGB36Regex @"^([A-Za-z])([A-Za-z])\s*([0-9]+)\s+([0-9]+)$" groups -> 
            let e = decodeOSGBNumber1 <| groups.[3].Value
            let n = decodeOSGBNumber1 <| groups.[4].Value
            Some <| makeOSGB36Grid (getChar1 groups) (getChar2 groups) e n
        | _ -> None

    let readOSGB36Grid (input:string) : OSGB36Grid = 
        match tryReadOSGB36Grid input with
        | Some(x) -> x
        | None -> failwith <| sprintf "readOSGB36Grid - could not read '%s'" input



    // Operates on WGS84Points i.e. Lat-Lon
    let haversineDistance (p1 : WGS84Point) (p2 : WGS84Point) : float<kilometer> = 
        let radius = 6371.000<kilometer>
        let lat1R = deg2rad (float p1.Latitude)
        let lat2R = deg2rad (float p2.Latitude)
        let latDeltaR = deg2rad (float (p2.Latitude-p1.Latitude))
        let lonDeltaR = deg2rad (float (p2.Longitude-p1.Longitude))
        let a = Math.Sin(latDeltaR /2.0) * Math.Sin(latDeltaR /2.0) + 
                Math.Cos(lat1R) * Math.Cos(lat2R) *
                Math.Sin(lonDeltaR / 2.0) * Math.Sin(lonDeltaR/2.0)
        
        let c = 2.0 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.0-a))

        let ans = radius * c
        ans

    let haversineDistanceOGSB36Point (p1:OSGB36Point) (p2:OSGB36Point) : float<kilometer> = 
        haversineDistance (osgb36PointToWGS84 p1) (osgb36PointToWGS84 p2) 

    let haversineDistanceOGSB36Grid (g1:OSGB36Grid) (g2:OSGB36Grid) : float<kilometer> = 
        haversineDistance (osgb36GridToWGS84 g1) (osgb36GridToWGS84 g2) 

