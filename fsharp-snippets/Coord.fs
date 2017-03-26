namespace Coord

open System



module Coord = 

    type metres = double    
    type ddegrees = double

    type OSGB36Point = { Eastings : metres; Northings : metres }
    type WGS84Point = { Latitude : ddegrees; Longitude : ddegrees }

    let deg2rad (d : float) = (Math.PI/180.0) * d

    let rad2deg (r : float) = (180.0/Math.PI) * r

    /// fromDMS :: Int -> Int -> Double -> DDegrees
    let fromDMS (d : int) (m : int) (s : float) = float d + float m / 60.0 + s / 3600.0

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

    let latlonToEN ({Latitude = phidd; Longitude = lamdd} : WGS84Point) = 
        let phi = deg2rad phidd
        let lam = deg2rad lamdd
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
        { Eastings = E; Northings = N }



    let enToLatLon ({Eastings = E; Northings = N} : OSGB36Point) =
        let rec makePhi p m = 
            if abs (N - N0 - m) < 0.01 then 
                p 
            else
                let phi_new = (N - N0 - m) / (a * F0) + p
                let Mnew   = equationC3 phi_new
                makePhi phi_new Mnew
        
        let phi1'       = (N - N0) / (a * F0) + phi0
        let M1         = equationC3 phi1'
        let phi'        = makePhi phi1' M1
        let (nu,rho,eta2)   = equationC2 phi'
        let tanPhi'         = tan phi'
        let cosPhi'         = cos phi'
        let secPhi'         = 1.0 / cosPhi'
        let tan2Phi'        = tanPhi' * tanPhi'
        let tan4Phi'        = tanPhi' * tanPhi' * tanPhi' * tanPhi'
        let tan6Phi'        = tanPhi' * tanPhi' * tanPhi' * tanPhi' * tanPhi' * tanPhi'
        let Em_E0          = E - E0

        let VII        = tanPhi' / (2.0 * rho * nu)
        let VIII       = tanPhi' / (24.0 * rho * nu ** 3.0) * (5.0 + 3.0 * tan2Phi' + eta2 - 9.0 * tan2Phi' * eta2)
        let IX         = tanPhi' / (720.0 * rho * nu ** 5.0) * (61.0 + 90.0 * tan2Phi' + 45.0 * tan4Phi')
        let X          = secPhi' / nu
        let XI         = secPhi' / (6.0 * nu ** 3.0) * (nu / rho + 2.0 * tan2Phi')
        let XII        = secPhi' / (120.0 * nu ** 5.0) * (5.0 + 28.0 * tan2Phi' + 24.0 * tan4Phi')
        let XIIA       = secPhi' / (5040.0 * nu ** 7.0) * (61.0 + 662.0 * tan2Phi' + 1320.0 * tan4Phi' + 720.0 * tan6Phi')
        let phi         = phi' - VII * (Em_E0 ** 2.0) + VIII * (Em_E0 ** 4.0) - IX * (Em_E0 ** 6.0)
        let lam         = lam0 + X * Em_E0 - XI * (Em_E0 ** 3.0) + XII * (Em_E0 ** 5.0) - XIIA * (Em_E0 ** 7.0)
        {Latitude = rad2deg phi; Longitude = rad2deg lam } 


    /// OS Grid refs, see
    /// https://en.wikipedia.org/wiki/Ordnance_Survey_National_Grid

   
    let decodeMajor (ch : char) = 
        match ch with
            | 'S' | 's' -> (0.0, 0.0)
            | 'T' | 't' -> (500000.0, 0.0)
            | 'N' | 'n' -> (0.0, 500000.0)
            | 'O' | 'o' -> (500000.0, 500000.0)
            | 'H' | 'h' -> (0.0, 1000000.0)
            | _         -> (-1000000.0, -1000000.0)

    let decodeMinor (ch : char) = 
        let shifti x  = 
            match x with
                | _ when x > 8  -> x-1
                | _             -> x
                /// (fun i -> i - 65)
        let fn =  shifti << (fun i -> i - 65) << int << Char.ToUpper
        let (n0, e1) = Math.DivRem(fn ch,5)
        let n1 = 4 - n0
        (float e1 * 100000.0, float n1 * 100000.0)

    let decodeAlpha (s : char) (t : char) = 
        let (eM, nM) = decodeMajor s
        let (em, nm) = decodeMinor t
        (eM + em, nM + nm)

    let decodeOSGridRef10 (m : char) (mm : char) east north = 
        let (majE, majN) = decodeAlpha m mm 
        {Eastings = majE + east; Northings = majN + north}
    
    let decodeOSGridRef6 (m : char) (mm : char) east north = 
        decodeOSGridRef10 m mm (east * 100.0) (north * 100.0) 

    /// Seq.toListmight be better...

    let numString (sz : int) (ss : string) = 
        if sz*2 = String.length ss then
            let left = ss.[0..sz-1]
            let right = ss.[sz..(sz*2-1)]
            (int left,int right)
        else
            (0,0)

    let ns (sz : int) (s1 : char list) = 
        let rec parse xs n ac = 
           match xs with
            | []-> (ac,[])
            | (c::cs) -> if n < sz then
                                let ac1 = ac * 10.0  + Char.GetNumericValue c
                                parse cs (n+1) ac1
                            else
                                (ac, xs)
        let (a,s2) = parse s1 0 0.0
        let (b,s3)  = parse s2 0 0.0
        (a,b) 

                                    
    let fromOSGridRef6 (ss : string) = 
        match Seq.toList ss with
            | (m :: mm :: xs) -> let (e,n) = ns 3 xs
                                 Some <| decodeOSGridRef6 m mm e n
            | _ -> None

    
    let fromOSGridRef10 (ss : string) = 
        match Seq.toList ss with
            | (m :: mm :: xs) -> let (e,n) = ns 5 xs
                                 Some <| decodeOSGridRef10 m mm e n
            | _ -> None    