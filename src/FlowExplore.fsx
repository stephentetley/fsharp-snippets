#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.CsvExtensions

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider



#load @"SL\ExcelProviderHelper.fs"
open SL.ExcelProviderHelper

open System.Text


type SaiTable = 
    ExcelFile< @"G:\work\common_data\SAINumbers.xlsx",
                SheetName = "SITES",
                HasHeaders = true,
                ForceString = true >

type SaiRow = SaiTable.Row



let getSaiDict () : Map<string,SaiRow> = 
    let dict () : IExcelProviderHelper<SaiTable, SaiRow> = 
         { new IExcelProviderHelper<SaiTable,SaiRow>
           with member this.GetTableRows imports = imports.Data 
                member this.IsBlankRow row = 
                    match row.GetValue(0) with null -> true | _ -> false }
    excelGetRows (dict ()) (new SaiTable()) 
        |> Seq.map (fun (row:SaiRow) -> row.InstCommonName, row)
        |> Map.ofSeq


type SitesTable = 
    ExcelFile< FileName = @"G:\work\Projects\ar-flowmeters\woodys-list.xlsx",
                SheetName = "Table",
                Range = "B2:E85",
                HasHeaders = true,
                ForceString = true >

            
type SitesRow = SitesTable.Row

let getSiteList () : SitesRow list = 
    let dict () : IExcelProviderHelper<SitesTable,SitesRow> = 
         { new IExcelProviderHelper<SitesTable,SitesRow>
           with member this.GetTableRows imports = imports.Data 
                member this.IsBlankRow row = 
                    match row.GetValue(0) with null -> true | _ -> false }
    excelGetRowsAsList (dict ()) (new SitesTable()) 

let getSiteName (source:string) : string = 
    let arr1 = source.Split('/')
    if arr1.Length > 2 then 
        String.concat "/" [ arr1.[0]; arr1.[1] ]
    else
        source

let getSaiNumber (dict:Map<string,SaiRow>) (name:string) : string = 
    match Map.tryFind name dict with
    | Some row -> row.InstReference
    | None -> ""

let test01 () : unit =
    let dict = getSaiDict ()
    List.iter (fun (row:SitesRow) -> 
                let siteName = getSiteName row.``Instrument Name (AI)``
                printfn "%s %s" siteName (getSaiNumber dict siteName)) 
        <| getSiteList ()

[<Literal>]
let OutSchema = 
    "Sai Number (string), Odyssey (string), Site Name (string), \
     Area (string), Inst Number (string), Inst Name (string), \
     Operational Contact (string), Work Center (string), \
     Site Address (string), Postcode (string)"

/// Setting Sample to the schema is a trick to generate Headers.
type OutputCsv = 
    CsvProvider< 
        Schema = OutSchema,
        Sample = OutSchema,
        HasHeaders = true>

type OutputRow = OutputCsv.Row

let outputRecord (dict:Map<string,SaiRow>) (row:SitesRow) : OutputRow = 
    let name1 = getSiteName row.``Instrument Name (AI)``
    let saiCode = getSaiNumber dict name1

    let withRow (defaultVal:'a) (fn:SaiRow -> 'a) : 'a = 
        match Map.tryFind name1 dict with
        | Some row -> fn row
        | None -> defaultVal

    OutputCsv.Row(
        saiNumber = saiCode,
        odyssey = sprintf "SAIREF = \"%s\" OR" saiCode,
        siteName = name1,
        area = row.Area,
        instNumber = row.Reference,
        instName = row.``Instrument Name (AI)``,
        operationalContact = withRow "" (fun x -> x.``Operational Responsibility``),
        workCenter = withRow "" (fun x -> x.``Work Centre`` ),
        siteAddress = withRow "" (fun x -> x.``Full Address``),
        postcode = withRow "" (fun x -> x.``Post Code``)
        )

let collateData (outPath:string) : unit = 
    let sais = getSaiDict ()
    let rows = getSiteList () |> List.map (outputRecord sais)
    let csvdata = new OutputCsv(rows)
    use sw = new System.IO.StreamWriter(outPath)
    // writeHeaders sw csvdata.Headers
    csvdata.Save(writer = sw, separator = ',', quote = '"')


let main () = 
    collateData @"G:\work\Projects\ar-flowmeters\site-list-simple.csv"

