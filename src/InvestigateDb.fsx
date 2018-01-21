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
open SL.Geo.Coord


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
    ; SaiNumber: string option
    ; OutletGriRef: string
    }

type LotusConsent = 
    { CommonName: string
    ; SaiNumber: string
    ; OutfallGridRef : OSGB36Point option 
    }

type CatsAsset = 
    { SaiNumber: string
      AssetName: string
      WorkCategory: string }

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
let catsAssetsTP () : CatsAsset list  = 
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
                    ; SaiNumber = match lc.SaiNumber with | None -> "" | Some s -> s
                    ; OutfallGridRef = tryMakeGridRefB lc.OutfallEasting lc.OutfallNorthing } : LotusConsent)
            distinct }
        |> Seq.toList

let lotusConsentsBySai (sai:String) : LotusConsent list  = 
    query { for lc in ctx.Main.LotusConsents do
            where (lc.SaiNumber = Some sai)
            sortBy (lc.CommonName)
            select ({ CommonName = lc.CommonName
                    ; SaiNumber = match lc.SaiNumber with | None -> "" | Some s -> s
                    ; OutfallGridRef = tryMakeGridRefB lc.OutfallEasting lc.OutfallNorthing } : LotusConsent)
            distinct }
        |> Seq.toList


let test01 () = 
    catsAssetsTP () |> List.iter (fun ca -> printfn "%s" ca.AssetName)

let test02 () = 
    lotusConsentsTP () |> List.iter (fun lc -> printfn "%s" lc.CommonName)

let test03 () = 
    lotusConsentsBySai "SAI00000225" |> List.iter (fun lc -> printfn "%s, %s" lc.CommonName lc.SaiNumber)