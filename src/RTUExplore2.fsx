#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider



#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper


//type AssetTable = 
//    ExcelFile< @"G:\work\Projects\rtu\AR-asset-expired-2011\Assets-os-export.xlsx",
//                SheetName = "Sheet1",
//                ForceString = true >
//type AssetRow = AssetTable.Row

//let getAssetRows () : AssetRow list = 
//    let dict : GetRowsDict<AssetTable, AssetRow> = 
//        { GetRows     = fun imports -> imports.Data 
//          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
//    excelTableGetRows dict (new AssetTable())

/// Favour Csv reader as ExcelProvider appearss to be transposing dates (???)
type AssetTable = 
    CsvProvider<  @"G:\work\Projects\rtu\AR-asset-expired-2011\Assets-os-export2.csv", 
                    HasHeaders = true >

type AssetRow = AssetTable.Row

let getAssetRows () : AssetRow list = 
    AssetTable.Load(@"G:\work\Projects\rtu\AR-asset-expired-2011\Assets-os-export2.csv").Rows 
        |> Seq.toList


type SaiTable = 
    ExcelFile< @"G:\work\Projects\rtu\AR-asset-expired-2011\SAINumbers.xlsx",
                SheetName = "SITE_LIST",
                ForceString = true >

type SaiRow = SaiTable.Row

let getSaiLookups () : Map<string,string> = 
    let dict : GetRowsDict<SaiTable, SaiRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SaiTable()) 
        |> List.map (fun row -> row.InstCommonName, row.InstReference)
        |> Map.ofList




let getSiteName (source:string) : string = 
    let arr1 = source.Split('/')
    if arr1.Length > 2 then 
        String.concat "/" [ arr1.[0]; arr1.[1] ]
    else
        source

let getSiteName1 (row:AssetRow) : string = getSiteName row.``Common Name``

let getHeaders (schema:string) : string = 
    let leftOf (needle:char) (source:string) = 
        source.Split(needle).[0]
        
    let arr = schema.Split(',')
    Array.map (fun s -> (leftOf '(' s).Trim() ) arr |> String.concat ","



[<Literal>]
let OutSchema = "Sai Number (string), Site(string), Manufacturer (string), Model (string), Specific Model (string), Serial Number (string), Installed From (string), Asset Status (string), Outstation Addr(string), Memo 1 (string)"


type OutputCsv = 
    CsvProvider< 
        Schema = OutSchema,
        HasHeaders = false>

let outputRecord (sais:Map<string,string>) (row:AssetRow) : OutputCsv.Row = 
    let sainum = 
        match Map.tryFind (getSiteName row.``Common Name``) sais with
        | Some str -> str
        | None -> "unknown"

    OutputCsv.Row(
        saiNumber = sainum,
        site=getSiteName row.``Common Name``, 
        manufacturer = row.Manufacturer,
        model = row.Model,
        specificModel = row.``Specific Model/Frame``,
        serialNumber = row.``Serial No``,
        installedFrom = row.``Installed From``,
        assetStatus = row.AssetStatus,
        outstationAddr = row.``RTU Configuration ID``,
        memo1 = row.``Memo Line 1`` )


let collateData (outPath:string) : unit = 
    let sais = getSaiLookups ()
    let csvdata = new OutputCsv(List.map (outputRecord sais)  <| getAssetRows ())
    use sw = new System.IO.StreamWriter(outPath)
    sw.WriteLine (getHeaders OutSchema)
    csvdata.Save(writer = sw, separator = ',', quote = '"')

let test01 () = 
    collateData @"G:\work\Projects\rtu\AR-asset-expired-2011\outstation-assets.csv"



