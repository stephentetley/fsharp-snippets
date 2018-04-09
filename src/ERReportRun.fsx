#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\\packages\SQLProvider.1.0.54\lib"
#r "FSharp.Data.SQLProvider.dll"
open FSharp.Data.Sql


open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ExcelProviderHelper.fs"
open SL.AnswerMonad
open SL.SqlUtils
open SL.SQLiteConn
open SL.ScriptMonad
open SL.CsvOutput
open SL.ExcelProviderHelper


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


/// "Make a proper tree"

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
      OverflowLocation: string          // descriptive string, not a grid ref
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



let uniqueItem (src:seq<'a>) : option<'a> = 
    match Seq.toList src with
    | [x] -> Some x
    | _ -> None

let getAssetName (uid:string) : string  = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = uid)       
        select (assets.AssetName:string)
        distinct 
    } |> uniqueItem |> Option.defaultValue "N/A"

let getParentSiteId (assetUid:string) : option<string> = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = assetUid)       
        select (assets.ParentSiteRef)
        distinct 
    } |> uniqueItem |> Option.flatten

type SiteRec = 
    { Uid: string
      SiteName: string
      SiteAddress: string
      PostCode: string }

let getSiteRec (uid:string) : option<SiteRec> = 
    query { 
        for sites in ctx.Main.AllSites do
        where (sites.Uid = uid)       
        select ({ Uid =  sites.Uid
                ; SiteName = sites.SiteName
                ; SiteAddress = Option.defaultValue "" <| sites.SiteAddress
                ; PostCode = Option.defaultValue "" <| sites.Postcode })
        distinct 
    } |> uniqueItem 

let getChildIds (siteUid:string) : string list = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.ParentSiteRef = Some siteUid && assets.AssetStatus = Some "OPERATIONAL")       
        select (assets.Uid)
        distinct 
        } |> Seq.toList

//let getParentSite (uid:string) : option<ParentSite> = 
//    let parentUid = 
//        query { 
//            for assets in ctx.Main.AllAssets do
//            where (assets.Uid = uid)       
//            select (assets.ParentSiteRef)
//            distinct 
//        } |> uniqueItem |> Option.flatten
//    let getParent1 (uid:string) = 
//        query { 
//            for sites in ctx.Main.AllSites do
//            where (sites.Uid = uid)       
//            select ({ Uid       = sites.Uid
//                    ; SiteName  = sites.SiteName
//                    ; Address   = Option.defaultValue "" <| sites.SiteAddress
//                    ; Postcode  = Option.defaultValue "" <| sites.Postcode })
//            distinct 
//        } |> uniqueItem
//    Option.bind getParent1 parentUid

let getAssetType (uid:string) : string = 
    query { 
        for assets in ctx.Main.AllAssets do
        where (assets.Uid = uid)       
        select (assets.AssetType:string option)
        distinct 
    } |> uniqueItem |> Option.flatten |> Option.defaultValue ""

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
            distinct 
        } |> uniqueItem |> Option.flatten 
    | "STW" -> 
        query { 
            for assets in ctx.Main.StwAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef None )
            distinct 
        } |> uniqueItem |> Option.flatten
    | "SPS" -> 
        query { 
            for assets in ctx.Main.SpsAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef assets.ConsentApplication )
            distinct 
        } |> uniqueItem |> Option.flatten
    | "ESO" -> 
        query { 
            for assets in ctx.Main.EsoAssets do
            where (assets.Uid = uid)       
            select  (
                makeConsent assets.ConsentRef None )
            distinct 
        } |> uniqueItem |> Option.flatten
    | _ -> None


let makeAsset (uid:string) : Asset = 
    let assetType = getAssetType uid
    { AssetUid = uid
    ; CommonName = getAssetName uid 
    ; AssetType = assetType
    ; Consent = getConsent uid assetType
    }

let processRow (row:AssetListRow) : unit =
    let uid = row.UID
    let assetName = getAssetName uid
     
    match getParentSiteId uid with
    | None -> printfn "ERROR: no parent for %s %s" uid assetName
    | Some pid -> 
        let kidsIds = getChildIds pid
        let site = getSiteRec pid
        let siteName = Option.defaultValue "" << Option.map (fun s -> s.SiteName) <| site
        printfn "%s => %s (kids:%A)" assetName siteName kidsIds

let temp02 () = 
    List.iter processRow << List.take 30 <|  getAssetList () 


let columnHeaders : string list = 
    [ "SAI of Related Asset"
    ; "Related AI2 Asset Name"
    ]

let tellOuputRow (uid:string) : CsvOutput<unit> = 
    tellRow [ tellString uid
            ]
