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

#load @"SL\Coord.fs"
#load @"SL\CsvOutput.fs"
open SL.Geo.Coord
open SL.CsvOutput

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


// Writing to xls has the same cardinality problems as SQL, we collapse
// consents / permits and their outfalls into a single string

let catsConsentString (source:CatsConsent list) : string = 
    let proc1 (cc:CatsConsent) : string = 
        sprintf "%s => %s" cc.PermitRef cc.OutletGridRef
    String.concat " & " <| List.map proc1 source

let catsConsentRefs (source:CatsConsent list) : string = 
    let proc1 (cc:CatsConsent) : string = 
        match tryReadOSGB36Grid cc.OutletGridRef with
        | Some ngr -> showOSGB36Grid ngr
        | None -> "??"
    String.concat " & " <| List.map proc1 source

let lotusConsentString (source:LotusConsent list) : string = 
    let proc1 (lc:LotusConsent) : string = 
        let gridref = 
            match lc.OutfallGridRef with 
            | None -> "??" 
            | Some pt -> showOSGB36Grid <| osgb36PointToGrid pt
        sprintf "%s => %s" lc.FullConsentName gridref
    String.concat " & " <| List.map proc1 source

let lotusConsentRefs (source:LotusConsent list) : string = 
    let proc1 (lc:LotusConsent) : string = 
        match lc.OutfallGridRef with 
        | None -> "??" 
        | Some pt -> showOSGB36Grid <| osgb36PointToGrid pt
    String.concat " & " <| List.map proc1 source

let stormDischargeString (source:StormDisPermit list) : string = 
    let proc1 (sdp:StormDisPermit) : string = 
        sprintf "%s => %s" sdp.PermitUrn sdp.OutletGridRef
    String.concat " & " <| List.map proc1 source

let stormDischargeRefs (source:StormDisPermit list) : string = 
    let proc1 (sdp:StormDisPermit) : string = 
        match tryReadOSGB36Grid sdp.OutletGridRef with
        | Some ngr -> showOSGB36Grid ngr
        | None -> "??"
    String.concat " & " <| List.map proc1 source

let AssetCollectedData () = 
    let assetList = makeAssets ()
    let outFile = @"G:work\Projects\events2\Asset-collected-data.csv"
    let headers = 
        [ "SAI Number"; "Asset Name"
        ; "Cats Consent Count"; "Cats NGRs"; "Cats Consents"
        ; "Lotus Consent Count"; "Lotus NGRs"; "Lotus Consents"
        ; "Storm Permit Count"; "Permit NGRs"; "Permit Outfalls" 
        ]
    let rowProc (a:Asset) : CellWriter list = 
        [ tellString        a.SaiNumber
        ; tellString        a.AssetName
        ; tellInt           <| List.length a.CatsConsents
        ; tellString        <| catsConsentRefs a.CatsConsents
        ; tellString        <| catsConsentString a.CatsConsents
        ; tellInt           <| List.length a.LotusConsents
        ; tellString        <| lotusConsentRefs a.LotusConsents
        ; tellString        <| lotusConsentString a.LotusConsents
        ; tellInt           <| List.length a.StormDisPermits
        ; tellString        <| stormDischargeRefs a.StormDisPermits
        ; tellString        <| stormDischargeString a.StormDisPermits
        ]
    let csvProc = 
        tellSheetWithHeaders headers assetList rowProc
    outputToNew csvProc outFile ","

type GisOutfall = 
    { Stc25Ref: string
      FunctionNode: string
      Osgb36GridRef: string }

let getGisOutfalls () : seq<GisOutfall>  = 
    query { for go in ctx.Main.GisOutfalls do
            select ({ Stc25Ref = go.Stc25Ref
                    ; FunctionNode = go.FunctionNode
                    ; Osgb36GridRef = go.Osgb36Gridref
                    } : GisOutfall)
            distinct } |> Seq.cast<GisOutfall>

let GisOutfalls () = 
    let assetList = getGisOutfalls ()
    let outFile = @"G:work\Projects\events2\GIS-outfalls.csv"
    let headers = 
        [ "STC25 Ref"; "Function Node"; "OSGB36 Gridref" ]
    let rowProc (a:GisOutfall) : CellWriter list = 
        [ tellString        a.Stc25Ref
        ; tellString        a.FunctionNode
        ; tellString        a.Osgb36GridRef
        ]
    let csvProc = 
        tellSheetWithHeaders headers assetList rowProc
    outputToNew csvProc outFile ","
