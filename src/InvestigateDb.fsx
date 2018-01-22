open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite


#I @"..\\packages\SQLProvider.1.0.54\lib"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql


#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\Coord.fs"
#load @"SL\ClosedXMLOutput.fs"
open SL.Geo.Coord
open SL.ClosedXMLOutput

let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\edmDB.sqlite3;Version=3"

type SqlDB = SqlDataProvider< 
              ConnectionString = ConnectionString1,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = ResolutionPath1,
              IndividualsAmount = 1000,
              UseOptionTypes = true >
let ctx = SqlDB.GetDataContext()


// SaiNumber and OutletGridRef can be missing
type StormDisPermit = 
    { AssetName: string
      SaiNumber: string option
      OutletGridRef: string
      DischargeDesc: string
      PermitUrn: string
      ReceivingWater: string }

type LotusConsent = 
    { CommonName: string
      SaiNumber: string
      OutfallGridRef : OSGB36Point option 
      FullConsentName: string
      ShortConsentName: string }

type CatsConsent = 
    { SaiNumber: string
      OutletGridRef: string
      PermitRef: string
      ToplevelPermitRef: string }

type CatsAsset = 
    { SaiNumber: string
      AssetName: string
      WorkCategory: string }

type Asset = 
    { SaiNumber: string
      AssetName: string
      WorkPlan: string      // aka CatsConsent:WorkCategory
      CatsConsents: CatsConsent list 
      LotusConsents: LotusConsent list
      StormDisPermits: StormDisPermit list
      }

// *** Helpers

let stringFromOption (source:string option) : string = 
    match source with | Some s -> s | None -> ""

let tryMakeGridRefA (east:Nullable<int>) (north:Nullable<int>) : OSGB36Point option = 
    match east.HasValue, north.HasValue with
    | true,true -> 
        if east.Value > 0 && north.Value > 0 then 
            Some {Easting = 1.0<meter> * float east.Value; Northing = 1.0<meter> * float north.Value}
        else None
    | _,_ -> None

let tryMakeGridRefB (east:int64 option) (north:int64 option) : OSGB36Point option = 
    match east, north with
    | Some (east1), Some (north1) -> 
        if east1 > 0L && north1 > 0L then 
            Some {Easting = 1.0<meter> * float east1; Northing = 1.0<meter> * float north1}
        else None
    | _, _ -> None

// SQLProvider clearly wins when it can be used...
let getCatsAssets () : CatsAsset list  = 
    query { for cc in ctx.Main.CatsConsents do
            sortBy (cc.AssetName)
            select ({ SaiNumber = cc.AssetSaiNumber
                    ; AssetName = cc.AssetName
                    ; WorkCategory = cc.WorkCategory}:CatsAsset)
            distinct }
        |> Seq.toList

let lotusConsentsTP () : LotusConsent list  = 
    query { for lc in ctx.Main.LotusConsents do
            sortBy (lc.CommonName)
            select ({ CommonName = lc.CommonName
                    ; SaiNumber = stringFromOption lc.SaiNumber
                    ; OutfallGridRef = tryMakeGridRefB lc.OutfallEasting lc.OutfallNorthing 
                    ; FullConsentName = stringFromOption lc.FullConsentName
                    ; ShortConsentName = stringFromOption lc.ShortConsentName
                    } : LotusConsent)
            distinct }
        |> Seq.toList

let getCatsConsentsFor (sai:String) : CatsConsent list  = 
    query { for cc in ctx.Main.CatsConsents do
            where (cc.AssetSaiNumber = sai)
            select ({ SaiNumber = cc.AssetSaiNumber
                    ; OutletGridRef = stringFromOption cc.OutletNgr
                    ; PermitRef = stringFromOption cc.PermitRef
                    ; ToplevelPermitRef = stringFromOption cc.ToplevelPermitRef
                    } : CatsConsent)
            distinct }
        |> Seq.toList

let getLotusConsentsFor (sai:String) : LotusConsent list  = 
    query { for lc in ctx.Main.LotusConsents do
            where (lc.SaiNumber = Some sai)
            sortBy (lc.CommonName)
            select ({ CommonName = lc.CommonName
                    ; SaiNumber = stringFromOption lc.SaiNumber 
                    ; OutfallGridRef = tryMakeGridRefB lc.OutfallEasting lc.OutfallNorthing 
                    ; ShortConsentName = stringFromOption lc.ShortConsentName
                    ; FullConsentName = stringFromOption lc.FullConsentName
                    } : LotusConsent)
            distinct }
        |> Seq.toList


let test01 () = 
    getCatsAssets () |> List.iter (fun ca -> printfn "%s" ca.AssetName)

let test02 () = 
    lotusConsentsTP () |> List.iter (fun lc -> printfn "%A" lc)

let test03 () = 
    getLotusConsentsFor "SAI00000225" |> List.iter (fun lc -> printfn "%s, %s" lc.CommonName lc.SaiNumber)

let getStormDisPermitsFor (assetName:String) : StormDisPermit list  = 
    query { for sdp in ctx.Main.StormDisPermits do
            where (sdp.AssetName = Some assetName)
            select ({ AssetName = stringFromOption sdp.AssetName
                    ; SaiNumber = sdp.SaiNumber
                    ; OutletGridRef = stringFromOption sdp.OutletGridRef
                    ; DischargeDesc = stringFromOption sdp.DischargeDecription
                    ; PermitUrn = stringFromOption sdp.PermitUrn
                    ; ReceivingWater = stringFromOption sdp.ReceivingWatercourse
                    } : StormDisPermit)
            distinct }
        |> Seq.toList


let makeAssets () : Asset list = 
    let catsAssets = getCatsAssets ()
    catsAssets 
        |> List.map (fun ca ->
                { SaiNumber = ca.SaiNumber
                ; AssetName = ca.AssetName
                ; WorkPlan = ca.WorkCategory
                ; CatsConsents = getCatsConsentsFor ca.SaiNumber
                ; LotusConsents = getLotusConsentsFor ca.SaiNumber
                ; StormDisPermits = getStormDisPermitsFor ca.AssetName
                } : Asset )

let test04 () = 
    makeAssets ()
        |> List.iter (fun a -> printfn "%s,%s,%i cats-consents,%i lotus-consents,%i dis-permits" 
                                        a.SaiNumber
                                        a.AssetName
                                        (List.length a.CatsConsents)
                                        (List.length a.LotusConsents)
                                        (List.length a.StormDisPermits)
                                        )
// Writing to xls has the same cardinality problems as SQL, we collapse
// consents / permits and their outfalls into a single string

let catsConsentString (source:CatsConsent list) : string = 
    let proc1 (cc:CatsConsent) : string = 
        sprintf "%s => %s" cc.PermitRef cc.OutletGridRef
    String.concat "; " <| List.map proc1 source


let lotusConsentString (source:LotusConsent list) : string = 
    let proc1 (lc:LotusConsent) : string = 
        let gridref = 
            match lc.OutfallGridRef with 
            | None -> "?" 
            | Some pt -> showOSGB36Grid <| osgb36PointToGrid pt
        sprintf "%s => %s" lc.FullConsentName gridref
    String.concat "; " <| List.map proc1 source

let stormDischargeString (source:StormDisPermit list) : string = 
    let proc1 (sdp:StormDisPermit) : string = 
        sprintf "%s => %s" sdp.PermitUrn sdp.OutletGridRef
    String.concat "; " <| List.map proc1 source

let main () = 
    let assetList = makeAssets ()
    let outFile = @"G:work\Projects\events2\Asset-collected-data.xlsx"
    let headers = 
        [ "SAI Number"; "Asset Name"
        ; "Cats Consent Count"; "Cats Consents"
        ; "Lotus Consent Count"; "Lotus Consents"
        ; "Storm Permit Count"; "Permit Outfalls" 
        ]
    let rowProc (a:Asset) : CellWriter list = 
        [ tellString        a.SaiNumber
        ; tellString        a.AssetName
        ; tellInt           <| List.length a.CatsConsents
        ; tellString        <| catsConsentString a.CatsConsents
        ; tellInt           <| List.length a.LotusConsents
        ; tellString        <| lotusConsentString a.LotusConsents
        ; tellInt           <| List.length a.StormDisPermits
        ; tellString        <| stormDischargeString a.StormDisPermits
        ]
    let xlsProc = 
        tellSheetWithHeaders headers assetList rowProc
    outputToNew xlsProc outFile "Assets" 