#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"


#load @"SL\CsvOutput.fs"
#load @"SL\ExcelProviderHelper.fs"
open SL.CsvOutput
open SL.ExcelProviderHelper

type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\enva\Siblings.xlsx",
                SheetName = "Siblings",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.InstReference with null -> false | _ -> true }

let getImportRows () : seq<ImportRow> = excelTableGetRowsSeq importTableDict (new ImportTable())

let isOperational (row:ImportRow) : bool = 
    row.AssetStatus.Equals("Operational", System.StringComparison.CurrentCultureIgnoreCase)

type SiteName = string
type InstName = string

type InstZero  = 
    { Inst: InstName
      Parent: SiteName }

let collectInstZeros () : seq<InstZero> = 
    getImportRows ()
        |> Seq.map (fun (row:ImportRow) -> { Inst = row.InstCommonName; Parent = row.SiteCommonName })



type SiteZeros = Map<SiteName, InstName list>


let collectSiteZeros () : SiteZeros = 
    let insertProc (row:ImportRow) (store:SiteZeros) = 
        if isOperational(row) then 
            let key = row.SiteCommonName
            let value = row.InstCommonName
            match Map.tryFind key store with
            | None -> Map.add key [value] store
            | Some(xs) -> Map.add key (value :: xs) store
        else store
    Seq.foldBack insertProc (getImportRows ()) Map.empty

type InstOne  = 
    { Inst: InstName
      Siblings: InstName list }

let reconcile (insts:seq<InstZero>) (store:SiteZeros) : seq<InstOne> = 
    let reconcile1 (inst:InstZero) : InstOne = 
        match Map.tryFind inst.Parent store with
        | None -> { Inst = inst.Inst; Siblings = [] }
        | Some (xs) -> { Inst = inst.Inst; Siblings = List.except [inst.Inst] xs }
    Seq.map reconcile1 insts


let headers = 
    [ "Common Name"
    ; "Siblings"
    ]



let makeOutputRow (instOne:InstOne) : RowWriter = 
    [ tellString        <| instOne.Inst
    ; tellString        <| String.concat "; " instOne.Siblings
    ]

let main () =
    let csvOutputPath = @"G:\work\Projects\events2\enva\siblings-output.csv"
    let instZeros = collectInstZeros ()
    let sitesStore = collectSiteZeros ()
    let insts = reconcile instZeros sitesStore 
    let writerProc = csvOutput {
        do! tellHeaders headers
        do! tellRecords insts makeOutputRow  }
    ignore <| runCsvOutput { Separator = "," } csvOutputPath writerProc    

    


