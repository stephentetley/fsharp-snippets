#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net46"
#r "System.Data.SQLite"
open System.Data.SQLite


#I @"..\packages\SQLProvider.1.1.41\lib\net451"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql


#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames


// #load @"SL\Coord.fs"
#load @"SL\CsvOutput.fs"
// open SL.Geo.Coord
open SL.CsvOutput

let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\eventsDB.sqlite3;Version=3"

type SqlDB = SqlDataProvider< 
              ConnectionString = ConnectionString1,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = ResolutionPath1,
              IndividualsAmount = 1000,
              UseOptionTypes = true >
let ctx = SqlDB.GetDataContext()


// Uid and OutletGridRef can be missing
type StormDisPermit = 
    { AssetName: string
      AssetUid: string option
      OutletGridRef: string
      DischargeDesc: string
      PermitUrn: string
      ReceivingWater: string }

type LotusConsent = 
    { CommonName: string
      AssetUid: string
      OutfallGridRef : OSGB36Point option 
      FullConsentName: string
      ShortConsentName: string }

type CatsConsent = 
    { AssetUid: string
      OutletGridRef: string
      PermitRef: string
      ToplevelPermitRef: string }

type CatsAsset = 
    { AssetUid: string
      AssetName: string
      WorkCategory: string }

type Asset = 
    { AssetUid: string
      AssetName: string
      WorkPlan: string      // aka CatsConsent:WorkCategory
      AssetOsgb36: string
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
            select ({ AssetUid = cc.AssetUid
                    ; AssetName = cc.AssetName
                    ; WorkCategory = cc.WorkCategory}:CatsAsset)
            distinct }
        |> Seq.toList

let lotusConsentsTP () : LotusConsent list  = 
    query { for lc in ctx.Main.LotusConsents do
            sortBy (lc.CommonName)
            select ({ CommonName = lc.CommonName
                    ; AssetUid = stringFromOption lc.AssetUid
                    ; OutfallGridRef = tryMakeGridRefB lc.OutfallEasting lc.OutfallNorthing 
                    ; FullConsentName = stringFromOption lc.FullConsentName
                    ; ShortConsentName = stringFromOption lc.ShortConsentName
                    } : LotusConsent)
            distinct }
        |> Seq.toList

let getCatsConsentsFor (uid:String) : CatsConsent list  = 
    query { for cc in ctx.Main.CatsConsents do
            where (cc.AssetUid = uid)
            select ({ AssetUid = cc.AssetUid
                    ; OutletGridRef = stringFromOption cc.OutletNgr
                    ; PermitRef = stringFromOption cc.PermitRef
                    ; ToplevelPermitRef = stringFromOption cc.ToplevelPermitRef
                    } : CatsConsent)
            distinct }
        |> Seq.toList

let getLotusConsentsFor (uid:String) : LotusConsent list  = 
    query { for lc in ctx.Main.LotusConsents do
            where (lc.AssetUid = Some uid)
            sortBy (lc.CommonName)
            select ({ CommonName = lc.CommonName
                    ; AssetUid = stringFromOption lc.AssetUid 
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
                    ; AssetUid = sdp.AssetUid
                    ; OutletGridRef = stringFromOption sdp.OutletGridRef
                    ; DischargeDesc = stringFromOption sdp.DischargeDecription
                    ; PermitUrn = stringFromOption sdp.PermitUrn
                    ; ReceivingWater = stringFromOption sdp.ReceivingWatercourse
                    } : StormDisPermit)
            distinct }
        |> Seq.toList

let getSiteNGRFor (assetName:String) : string option  = 
    let finalize xs = match xs with | x :: _ -> x | _ -> None
    query { for site in ctx.Main.BaseSites do
            where (site.CommonName = assetName)
            select (site.SiteNgr: string option)
            distinct }
        |> Seq.toList |> finalize


let makeAssets () : Asset list = 
    let catsAssets = getCatsAssets ()
    catsAssets 
        |> List.map (fun ca ->
                { AssetUid = ca.AssetUid
                ; AssetName = ca.AssetName
                ; WorkPlan = ca.WorkCategory
                ; AssetOsgb36 = stringFromOption <| getSiteNGRFor ca.AssetName
                ; CatsConsents = getCatsConsentsFor ca.AssetUid
                ; LotusConsents = getLotusConsentsFor ca.AssetUid
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
        match tryReadOSGB36Point cc.OutletGridRef with
        | Some ngr -> showOSGB36Point ngr
        | None -> "??"
    String.concat " & " <| List.map proc1 source

let lotusConsentString (source:LotusConsent list) : string = 
    let proc1 (lc:LotusConsent) : string = 
        let gridref = 
            match lc.OutfallGridRef with 
            | None -> "??" 
            | Some pt -> showOSGB36Point pt
        sprintf "%s => %s" lc.FullConsentName gridref
    String.concat " & " <| List.map proc1 source

let lotusConsentRefs (source:LotusConsent list) : string = 
    let proc1 (lc:LotusConsent) : string = 
        match lc.OutfallGridRef with 
        | None -> "??" 
        | Some pt -> showOSGB36Point pt
    String.concat " & " <| List.map proc1 source

let stormDischargeString (source:StormDisPermit list) : string = 
    let proc1 (sdp:StormDisPermit) : string = 
        sprintf "%s => %s" sdp.PermitUrn sdp.OutletGridRef
    String.concat " & " <| List.map proc1 source

let stormDischargeRefs (source:StormDisPermit list) : string = 
    let proc1 (sdp:StormDisPermit) : string = 
        match tryReadOSGB36Point sdp.OutletGridRef with
        | Some ngr -> showOSGB36Point ngr
        | None -> "??"
    String.concat " & " <| List.map proc1 source

let AssetCollectedData () = 
    let assetList = makeAssets ()
    let outFile = @"G:work\Projects\events2\Asset-collected-data.csv"
    let headers = 
        [ "Asset Uid"; "Asset Name"; "Asset OSGB36"
        ; "Cats Consent Count"; "Cats NGRs"; "Cats Consents"
        ; "Lotus Consent Count"; "Lotus NGRs"; "Lotus Consents"
        ; "Storm Permit Count"; "Permit NGRs"; "Permit Outfalls" 
        ]
    let rowProc (a:Asset) : CellWriter list = 
        [ tellString        a.AssetUid
        ; tellString        a.AssetName
        ; tellString        a.AssetOsgb36
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
        writeRecordsWithHeaders headers assetList rowProc
    outputToNew {Separator=","} csvProc outFile

type Outfall = 
    { Stc25Ref: string
      FunctionNode: string
      Osgb36GridRef: string }

let getOutfalls () : seq<Outfall>  = 
    query { for go in ctx.Main.Outfalls do
            select ({ Stc25Ref = go.Stc25Ref
                    ; FunctionNode = go.FunctionNode
                    ; Osgb36GridRef = go.Osgb36Gridref
                    } : Outfall)
            distinct } |> Seq.cast<Outfall>

let Outfalls () = 
    let assetList = getOutfalls ()
    let outFile = @"G:work\Projects\events2\outfalls-output.csv"
    let headers = 
        [ "STC25 Ref"; "Function Node"; "OSGB36 Gridref" ]
    let rowProc (a:Outfall) : CellWriter list = 
        [ tellString        a.Stc25Ref
        ; tellString        a.FunctionNode
        ; tellString        a.Osgb36GridRef
        ]
    let csvProc = 
        writeRecordsWithHeaders headers assetList rowProc
    outputToNew {Separator=","} csvProc outFile
