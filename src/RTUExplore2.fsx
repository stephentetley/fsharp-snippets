#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.CsvExtensions

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider



#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper




/// Favour Csv reader as ExcelProvider appears to be transposing dates to MM/dd/yyyy (???)
type AssetTable = 
    CsvProvider<  @"G:\work\Projects\rtu\AR-asset-expired-2011\mmims-2010-to-2012.csv", 
                    HasHeaders = true >

type AssetRow = AssetTable.Row

let getAssetRows () : AssetRow list = 
    AssetTable.Load(@"G:\work\Projects\rtu\AR-asset-expired-2011\mmims-2010-to-2012.csv").Rows 
        |> Seq.toList

let file1 = 
    AssetTable.Load(@"G:\work\Projects\rtu\AR-asset-expired-2011\mmims-2010-to-2012.csv").Headers


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
let OutSchema = 
    "Sai Number (string), Site(string), Manufacturer (string), \
     Model (string), Specific Model (string), Serial Number (string), \
     Installed From (string), Asset Status (string), Memo 1 (string), \
     Memo 2 (string)"


/// Setting Sample to the schema is a trick to generate Headers.
type OutputCsv = 
    CsvProvider< 
        Schema = OutSchema,
        Sample = OutSchema,
        HasHeaders = true>

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
        specificModel = row.``Specific Model_Frame``,
        serialNumber = row.``Serial No``,
        installedFrom = row.``Installed From``,
        assetStatus = row.AssetStatus,
        memo1 = row.``Memo Line 1`` , 
        memo2 = row.``Memo Line 2`` ) 



/// TODO - should optQuote
let writeHeaders (sw:System.IO.StreamWriter) (headers:string [] option) : unit = 
    let leftName(source:string) = source.Split('(').[0].Trim()
    match headers with 
    | None -> ()
    | Some schema -> 
        let line = Array.map leftName schema |> String.concat ","
        sw.WriteLine (line)


let collateData (outPath:string) : unit = 
    let sais = getSaiLookups ()
    let csvdata = new OutputCsv(List.map (outputRecord sais)  <| getAssetRows ())
    use sw = new System.IO.StreamWriter(outPath)
    writeHeaders sw csvdata.Headers
    csvdata.Save(writer = sw, separator = ',', quote = '"')

let test01 () = 
    collateData @"G:\work\Projects\rtu\AR-asset-expired-2011\outstation-mmims-2010-2012.csv"



