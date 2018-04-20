#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net45"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\SQLProvider.1.1.41\lib\net451"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql


#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames


#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\Coord.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.CsvOutput
open SL.ExcelProviderHelper
open SL.Geo.Coord


let [<Literal>] ResolutionPath1 = __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
let [<Literal>] ConnectionString1 = @"Data Source=G:\work\Projects\events2\er-report\erReportDB.sqlite3;Version=3"

type SqlDB = SqlDataProvider< 
              ConnectionString = ConnectionString1,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = ResolutionPath1,
              IndividualsAmount = 1000,
              UseOptionTypes = true >
let ctx = SqlDB.GetDataContext()



// ********** SCRIPT **********
type Script<'a> = ScriptMonad<SQLiteConnParams,'a>

let withConnParams (fn:SQLiteConnParams -> Script<'a>) : Script<'a> = 
    scriptMonad.Bind (ask (), fn)

let liftSQLiteConn (sql:SQLiteConn<'a>) : Script<'a> = 
    withConnParams <| fun conn -> liftAnswer <| runSQLiteConn conn sql


// ********** DATA SETUP **********

type AssetListTable = 
    ExcelFile< @"G:\work\Projects\events2\er-report\SiteList.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type AssetListRow = AssetListTable.Row

let getAssetList () : AssetListRow list = 
    let dict : GetRowsDict<AssetListTable, AssetListRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new AssetListTable())

let temp01 () = 
    List.iter (fun (asset:AssetListRow) -> printfn "%s" asset.Name) <| getAssetList () 


/// "Make a proper tree" ???

type ParentSite = 
    { SiteUid: string
      SiteName: string
      Address: string
      Postcode: string } 

type Consent = 
    { DocumentRef: string
      Application: string }

type ESO = 
    { EsoUid: string 
      EsoName: string 
      OpStatus: string
      GridRef: string
      ConsentRef: string
      OverflowPoint: string          // descriptive string, not a grid ref
      ScreensType: string }


// Probably wrong - want a union of SPS, CSO, ...
type Asset = 
    { AssetUid: string
      CommonName: string
      AssetType: string
      Consent: option<Consent>
      }

type Site = 
    { SiteUid: string
      SiteName: string
      Address: string
      Postcode: string
      Assets: Asset list }

// TODO - use exactlyOneOrDefault instead...
//let uniqueItem (src:seq<'a>) : option<'a> = 
//    match Seq.toList src with
//    | [x] -> Some x
//    | _ -> None

let getAssetName (uid:string) : string  = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = uid)       
        select (Some <| assets.AssetName)
        exactlyOneOrDefault 
    } |> Option.defaultValue "N/A"



let getParentSiteId (assetUid:string) : option<string> = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = assetUid)       
        select (assets.ParentSiteRef)
        exactlyOneOrDefault 
    } 

let getSiblingIds (assetUid:string) (siblingType:string) : string list = 
    let sk (parentId:string) = 
        query {
            for row in ctx.Main.AllAssets do
            where (row.ParentSiteRef = Some parentId 
                    && row.AssetType = Some siblingType 
                    && row.AssetStatus = Some "OPERATIONAL")       
            select (row.Uid)
        } |> Seq.toList
    match getParentSiteId assetUid with
    | None -> []
    | Some pid -> sk pid

let getSiblingSpsId (assetUid:string) : option<string> = 
    match getSiblingIds assetUid "SPS" with
    | [x] -> Some x
    | _ -> None

type SiteRec = 
    { Uid: string
      SiteName: string
      SiteAddress: string
      PostCode: string }

let mapSiteRec (dbRec:SqlDB.dataContext.``main.all_sitesEntity``) : SiteRec = 
    { Uid =  dbRec.Uid
      SiteName = dbRec.SiteName
      SiteAddress = Option.defaultValue "" <| dbRec.SiteAddress
      PostCode = Option.defaultValue "" <| dbRec.Postcode }


let getSiteRec (uid:string) : option<SiteRec> = 
    query { 
        for sites in ctx.Main.AllSites do
        where (sites.Uid = uid)       
        select (Some { Uid =  sites.Uid
                     ; SiteName = sites.SiteName
                     ; SiteAddress = Option.defaultValue "" <| sites.SiteAddress
                     ; PostCode = Option.defaultValue "" <| sites.Postcode })
        exactlyOneOrDefault 
    } 




let getChildIds (siteUid:string) : string list = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.ParentSiteRef = Some siteUid && assets.AssetStatus = Some "OPERATIONAL")       
        select (assets.Uid)
        distinct 
        } |> Seq.toList


let getAssetType (uid:string) : string option = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = uid)       
        select (assets.AssetType:string option)
        exactlyOneOrDefault
    } 

let getConsent (uid:string) (siteType:string) : option<Consent> = 
    let makeConsent (name:option<string>) (watercourse:option<string>) : option<Consent> = 
        Option.map (fun ss ->  { DocumentRef = ss
                               ; Application = Option.defaultValue "" watercourse }) name
        
    match siteType with
    | "CSO" -> 
        query { 
            for assets in ctx.Main.CsoAssets do
            where (assets.Uid = uid)       
            select (
                makeConsent assets.ConsentRef assets.ConsentApplication )
            exactlyOneOrDefault 
        } 
    | "STW" -> 
        query { 
            for assets in ctx.Main.StwAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef None )
            exactlyOneOrDefault 
        } 
    | "SPS" -> 
        query { 
            for assets in ctx.Main.SpsAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef assets.ConsentApplication )
            exactlyOneOrDefault 
        } 
    | "ESO" -> 
        query { 
            for assets in ctx.Main.EsoAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef None )
            exactlyOneOrDefault 
        } 
    | _ -> None

let makeOSGB36 (easting:int64) (northing:int64) : OSGB36Point = 
    let conv (i:int64) : float<meter> = float i * 1.0<meter>;
    {Easting = conv easting; Northing = conv  northing}

let getOutletNGR (uid:string) (siteType:string) : option<string> = 
    try
        match siteType with
        | "SPS" -> 
            let ans = 
                query { 
                    for row in ctx.Main.SpsAssets do
                    where (row.Uid = uid)       
                    select (row.EoDischargeEasting, row.EoDischargeNorthing )
                    exactlyOneOrDefault 
                } 
            match ans with
            | Some e, Some n -> Some << showOSGB36Point <| makeOSGB36 e n
            | _ , _ -> None
        | "CSO" -> 
            let ans = 
                query { 
                    for row in ctx.Main.CsoAssets do
                    where (row.Uid = uid)       
                    select (row.DischargeEasting, row.DischargeNorthing)
                    exactlyOneOrDefault 
                } 
            match ans with
            | Some e, Some n -> Some << showOSGB36Point <| makeOSGB36 e n
            | _ , _ -> None

        | _ -> None
    with
    | _ ->
        printfn "Failed getOutletNGR %s, returning None" uid
        None

let getReceivingWatercourse (uid:string) (siteType:string) : option<string> = 
    try
        match siteType with
        | "CSO" -> 
            let ans = 
                query { 
                    for row in ctx.Main.CsoAssets do
                    where (row.Uid = uid)       
                    select (row.OverflowLocation, row.ReceivingWatercourse )
                    exactlyOneOrDefault 
                } 
            match ans with
            | _, ans1 -> ans1

        | "STW" -> None 
        | "SPS" -> 
            query { 
                for row in ctx.Main.SpsAssets do
                where (row.Uid = uid)       
                select  (row.EoReceivingWatercourse)
                exactlyOneOrDefault 
            } 
        | "ESO" -> None
        | "DTK" -> 
            query { 
                for row in ctx.Main.DtkAssets do
                where (row.Uid = uid)       
                select  (row.ReceivingWatercourse)
                exactlyOneOrDefault 
            } 
        | _ -> None
    with
    | _ -> 
        printfn "Failed getReceivingWatercourse %s, returning None" uid
        None


type ScreenParams = 
    { ScreenType: string
      ScreenAperture: string
      Flow: string
      FlowUnits: string }


let getScreenParams(uid:string) : option<ScreenParams> = 
    let sk (xs:ScreenParams list) : option<ScreenParams> = 
        match xs with
        | [x] -> Some x
        | _ -> None
    query { 
        for row in ctx.Main.Screens do
        where (row.InstUid = Some uid)       
        select ({ ScreenType = Option.defaultValue "" row.ScreenType
                  ScreenAperture = Option.defaultValue "" row.ScreenAperture
                  Flow = Option.defaultValue "" row.Flow
                  FlowUnits  = Option.defaultValue "" row.FlowUnits }) 
    } |> Seq.toList |> sk


let getStormTankCapacity (uid:string) : option<decimal> = 
    let sk (xs: (option<decimal>) list) : option<decimal> = 
        match xs with
        | [x] -> x
        | _ -> None
    query { 
        for row in ctx.Main.Tanks do
        where (row.InstUid = Some uid && row.TankType = "WW_STORM_SEDIMENTATION")       
        select (row.TankCapacity) 
    } |> Seq.toList |> sk

//let processRow (row:AssetListRow) : unit =
//    let uid = row.UID
//    let assetName = getAssetName uid
     
//    match getParentSiteId uid with
//    | None -> printfn "ERROR: no parent for %s %s" uid assetName
//    | Some pid -> 
//        let kidsIds = getChildIds pid
//        let site = getSiteRec pid
//        let siteName = Option.defaultValue "" << Option.map (fun s -> s.SiteName) <| site
//        printfn "%s => %s (kids:%A)" assetName siteName kidsIds




let columnHeaders : string list = 
    [ "SAI of Related Asset"
    ; "Related AI2 Asset Name"
    ; "Address of related AI asset"
    ; "Discharge Point Name"
    ; "Outlet NGR"
    ; "STC 25 of discharge point (if known)"
    ; "Receiving Water / Environment"
    ; "Screen Type"
    ; "Screen Aperture size"
    ; "Screen Flow"
    ; "Storm Tank Capacity"
    ]

type OutputRec = 
    { Uid: string
      AssetName: string
      AssetAddress: string
      DischargePointName: string
      OutletNGR: string
      DischargeSTC25: string 
      ReceivingWatercourse: string
      ScreenParams: option<ScreenParams>
      StormTankCapacity: option<decimal> }


let tellOuputRow (orec:OutputRec) : RowWriter = 
    let screenFlow (x:ScreenParams) : string = 
        match x.Flow, x.FlowUnits with
        | "", _ -> ""
        | s, "LITRES PER SECOND" -> s
        | s, t -> sprintf "%s %s" s t

    [ tellString orec.Uid
    ; tellString orec.AssetName
    ; tellString orec.AssetAddress
    ; tellString orec.DischargePointName
    ; tellString orec.OutletNGR
    ; tellString orec.DischargeSTC25
    ; tellString orec.ReceivingWatercourse
    ; tellString << Option.defaultValue "" << Option.map (fun (x:ScreenParams) -> x.ScreenType) <| orec.ScreenParams
    ; tellString << Option.defaultValue "" << Option.map (fun (x:ScreenParams) -> x.ScreenAperture) <| orec.ScreenParams
    ; tellString << Option.defaultValue "" << Option.map (fun (x:ScreenParams) -> screenFlow x) <| orec.ScreenParams
    ; tellOption tellDecimal orec.StormTankCapacity
    ]

let makeOutputRec (uid:string) : OutputRec = 
    printfn "%s" uid
    let parentId    = getParentSiteId uid   |> Option.defaultValue "n/a"
    let assetType   = getAssetType uid      |> Option.defaultValue "n/a" 
    let screenParams = getScreenParams uid
    { Uid = uid
      AssetName = getAssetName uid 
      AssetAddress = getSiteRec parentId
                        |> Option.map (fun x -> x.SiteAddress) 
                        |> Option.defaultValue "" 
      DischargePointName = ""
      OutletNGR = getOutletNGR uid assetType |> Option.defaultValue "" 
      DischargeSTC25 = ""
      ReceivingWatercourse = getReceivingWatercourse uid assetType |> Option.defaultValue "" 
      ScreenParams = getScreenParams uid
      StormTankCapacity = getStormTankCapacity uid
      }

let RunReport () = 
    // let xs =  List.take 30 <| getAssetList ()
    let xs = getAssetList ()
    let assetList = List.map (fun (x:AssetListRow) -> makeOutputRec x.UID) <| xs 
    let outFile = @"G:\work\Projects\events2\er-report\output1.csv"
    let csvProc = 
        writeRecordsWithHeaders columnHeaders assetList tellOuputRow
    outputToNew {Separator=","} csvProc outFile

let main () = 
    RunReport ()