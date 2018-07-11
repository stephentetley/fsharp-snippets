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


/// "ACORN PARK/STW" => "SAI0125"
let getSaiLookups () : Map<string,string> = 
    let dict : GetRowsDict<SaiTable, SaiRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SaiTable()) 
        |> List.map (fun row -> row.InstCommonName, row.InstReference)
        |> Map.ofList

let getAssetDict () : Map<string,SaiRow> = 
    let dict : GetRowsDict<SaiTable, SaiRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new SaiTable()) 
        |> List.map (fun row -> row.InstReference, row)
        |> Map.ofList



type RtsTable = 
    CsvProvider<  @"G:\work\Projects\rtu\AR-asset-expired-2011\rts-data.csv", 
                    HasHeaders = true,
                    IgnoreErrors = true >

type RtsRow = RtsTable.Row

let getRtsData () : Map<string, RtsRow> = 
    let csv = RtsTable.Load( @"G:\work\Projects\rtu\AR-asset-expired-2011\rts-data.csv")
    csv.Rows 
        |> Seq.map (fun row -> (row.``OD name``, row))
        |> Map.ofSeq



let getSiteName (source:string) : string = 
    let arr1 = source.Split('/')
    if arr1.Length > 2 then 
        String.concat "/" [ arr1.[0]; arr1.[1] ]
    else
        source

let getSiteName1 (row:AssetRow) : string = getSiteName row.``Common Name``

/// Can't do much about column headers they are auto-printed if HasHeaders = true

//let getHeaders (schema:string) : string = 
//    let leftOf (needle:char) (source:string) = 
//        source.Split(needle).[0]
        
//    let arr = schema.Split(',')
//    Array.map (fun s -> (leftOf '(' s).Trim() ) arr |> String.concat ","



[<Literal>]
let OutSchema = 
    "Sai Number (string), Odyssey (string), Site Name(string), Site Type (string), \
     Pli Number (string), AI2 Install Date (string), Outstation Name (string), \
     AI2 Path (string), Grid Ref (string), \
     Operational Contact (string), Work Center (string), \
     Site Address (string), Postcode (string), \
     OS Type (string), RTU Address (string), OS Comment (string), \
     AI2 Manufacturer (string), AI2 Model (string), AI2 Model or Frame (string)"


/// Setting Sample to the schema is a trick to generate Headers.
type OutputCsv = 
    CsvProvider< 
        Schema = OutSchema,
        Sample = OutSchema,
        HasHeaders = true>

let genOdyssey (saiNumber:string) : string = 
    sprintf "SAIREF = \"%s\" OR" saiNumber

let osAddrRepair (osAddr:string) :string = 
    let arr = osAddr.Split(',')
    if arr.Length = 2 then 
        sprintf "%s, %s" (arr.[0].Trim()) (arr.[1].Trim())
    else 
        sprintf "' %s" osAddr

let commonNamePath (osCommonName:string) : string = 
    let arr = osCommonName.Split('/')
    if arr.Length > 3 then 
        let stop = arr.Length - 2 // drop last 
        let arr1 = arr.[2..stop]
        String.concat "/" arr1
    else 
        ""



let outputRecord (assets:Map<string,SaiRow>) 
                    (sais:Map<string,string>) (rts:Map<string,RtsRow>) 
                    (currentRow:AssetRow) : OutputCsv.Row = 
    let sainum = 
        match Map.tryFind (getSiteName currentRow.``Common Name``) sais with
        | Some str -> str
        | None -> "unknown"


    let withSaiRow (defaultVal:'a) (fn :SaiRow -> 'a) : 'a = 
        match Map.tryFind sainum assets with
        | Some row -> fn row
        | None -> defaultVal

    let withRtsRow (defaultVal:'a) (fn :RtsRow -> 'a) : 'a = 
        match Map.tryFind sainum rts with
        | Some row -> fn row
        | None -> defaultVal

    OutputCsv.Row(
        saiNumber = sainum,
        odyssey = genOdyssey sainum, 
        siteName = getSiteName currentRow.``Common Name``, 
        siteType = withSaiRow "" (fun row -> row.AssetType),
        pliNumber = currentRow.Reference,
        ai2InstallDate = currentRow.``Installed From``, 
        outstationName = withRtsRow "" (fun row -> row.``OS name``),
        ai2Path = commonNamePath currentRow.``Common Name``,
        gridRef = withSaiRow "" (fun row -> row.LocationReference),
        operationalContact = withSaiRow "" (fun row -> row.``Operational Responsibility``),
        workCenter = withSaiRow "" (fun row -> row.``Work Centre``),
        siteAddress = withSaiRow "" (fun row -> row.``Full Address``),
        postcode = withSaiRow "" (fun row -> row.``Post Code``),
        osType = withRtsRow "" (fun row -> row.``OS type``),
        rtuAddress = withRtsRow "" (fun row -> osAddrRepair row.``OS Addr``),
        osComment = withRtsRow "" (fun row -> row.``OS comment``),
        ai2Manufacturer = currentRow.Manufacturer,
        ai2Model = currentRow.Model,
        ai2ModelOrFrame = currentRow.``Specific Model_Frame``
        )
        


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
    let assets = getAssetDict ()
    let rts = getRtsData ()
    let csvdata = new OutputCsv(List.map (outputRecord assets sais rts)  <| getAssetRows ())
    use sw = new System.IO.StreamWriter(outPath)
    // writeHeaders sw csvdata.Headers
    csvdata.Save(writer = sw, separator = ',', quote = '"')

let test01 () = 
    collateData @"G:\work\Projects\rtu\AR-asset-expired-2011\outstation-mmims-2010-2012.csv"



