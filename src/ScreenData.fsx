#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\Newtonsoft.Json.11.0.2\lib\net45"
#r "Newtonsoft.Json"
open Newtonsoft.Json

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#load @"SL\ExcelProviderHelper.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\JsonOutput.fs"
open SL.ExcelProviderHelper
open SL.CsvOutput
open SL.JsonOutput


type ImportTable = 
    ExcelFile< @"G:\work\Projects\events2\enva\SCREEN_DATA.xlsx",
                SheetName = "Screens",
                ForceString = true >

type ImportRow = ImportTable.Row

let importTableDict : GetRowsDict<ImportTable, ImportRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }

let getImportRows () : seq<ImportRow> = excelTableGetRowsSeq importTableDict (new ImportTable())


type ParentRefTable = 
    ExcelFile< @"G:\work\Projects\events2\enva\SCREEN_PARENT_REF.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

type ParentRefRow = ParentRefTable.Row

let parentRefTableDict : GetRowsDict<ParentRefTable, ParentRefRow> = 
    { GetRows     = fun imports -> imports.Data 
      NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }

let getParentRefs () : Map<string,string> = 
    let add1 (store:Map<string,string>) (row:ParentRefRow) : Map<string,string> = 
        Map.add row.``Common Name`` row.Reference store
    excelTableGetRowsSeq parentRefTableDict (new ParentRefTable()) 
        |> Seq.fold add1 Map.empty


let isOperational (row:ImportRow) : bool = 
    row.AssetStatus.Equals("Operational", System.StringComparison.CurrentCultureIgnoreCase)

//type SiteName = string
//type InstName = string

//type InstZero  = 
//    { Inst: InstName
//      Parent: SiteName }

//let collectInstZeros () : seq<InstZero> = 
//    getImportRows ()
//        |> Seq.map (fun (row:ImportRow) -> { Inst = row.InstCommonName; Parent = row.SiteCommonName })



//type SiteZeros = Map<SiteName, InstName list>


//let collectSiteZeros () : SiteZeros = 
//    let insertProc (row:ImportRow) (store:SiteZeros) = 
//        if isOperational(row) then 
//            let key = row.SiteCommonName
//            let value = row.InstCommonName
//            match Map.tryFind key store with
//            | None -> Map.add key [value] store
//            | Some(xs) -> Map.add key (value :: xs) store
//        else store
//    Seq.foldBack insertProc (getImportRows ()) Map.empty

//type InstOne  = 
//    { Inst: InstName
//      Siblings: InstName list }

//let reconcile (insts:seq<InstZero>) (store:SiteZeros) : seq<InstOne> = 
//    let reconcile1 (inst:InstZero) : InstOne = 
//        match Map.tryFind inst.Parent store with
//        | None -> { Inst = inst.Inst; Siblings = [] }
//        | Some (xs) -> { Inst = inst.Inst; Siblings = List.except [inst.Inst] xs }
//    Seq.map reconcile1 insts


//let headers = 
//    [ "Common Name"
//    ; "Siblings"
//    ]



//let makeOutputRow (instOne:InstOne) : RowWriter = 
//    [ tellString        <| instOne.Inst
//    ; tellString        <| String.concat "; " instOne.Siblings
//    ]



let parentSite (commonName:string) : string = 
    let arr = commonName.Split('/')
    if arr.Length >= 2 then 
        arr.[0] + "/" + arr.[1]
    else commonName

let functionalLocation (commonName:string) : string = 
    let arr = commonName.Split('/')
    let right = arr.Length - 2 
    if arr.Length > 3 then 
        arr.[2] + "/" + arr.[right]
    else ""

let parentScreen (commonName:string) : string = 
    let rec build ac xs = 
        match xs with
        | [] -> String.concat "/" <| List.rev ac
        | [s] -> String.concat "/" <| List.rev ac
        | s :: ss -> build (s::ac) ss
    commonName.Split('/') |> Array.toList |> build []
    
let findParentRef (commonName:string) (dict:Map<string,string>) : string = 
    let parentId = parentScreen commonName
    match Map.tryFind parentId dict with
    | None -> ""
    | Some s -> s

let temp01 () = 
    let src = @"ABERFORD ROAD/NO 1 CSO/STORM TREATMENT/SCREENING/NO 1 STORM SCREEN/EQUIPMENT: SCREENS"
    printfn "%s\n    => %s" (parentSite src) (functionalLocation src)
    printfn "%s" (parentScreen src)

let csvHeaders : string list = 
    [ "Equipment Ref (PLI)"
    ; "Site Name"
    ; "Functional Location"
    ; "Parent Ref (SAI)"
    ; "Common Name"
    ; "Installed From"
    ; "Manufacturer"
    ; "Model"
    ; "Asset Status"
    ; "Location Ref"
    ; "Screen Aperture Size"
    ; "Screen Type"
    ]

let rowToCsv (row:ImportRow) (refs:Map<string,string>) : RowWriter = 
    [ tellQuotedString      <| row.Reference
    ; tellQuotedString      << parentSite <| row.``Common Name``
    ; tellQuotedString      << functionalLocation <| row.``Common Name``
    ; tellQuotedString      <| findParentRef row.``Common Name`` refs
    ; tellQuotedString      <| row.``Common Name``
    ; tellQuotedString      <| row.``Installed From``
    ; tellQuotedString      <| row.Manufacturer
    ; tellQuotedString      <| row.Model
    ; tellQuotedString      <| row.AssetStatus
    ; tellQuotedString      <| row.``Loc.Ref.``
    ; tellQuotedString      <| row.``Screen Aperture Size mm``
    ; tellQuotedString      <| row.``Screen Type``
    ]

// TODO reference should be SAI number from one above in the hierarchy  
// (Not sure how to get this)
let temp02 () =
    let parentDict = getParentRefs ()
    getImportRows () 
        |> Seq.take 10
        |> Seq.iter (fun a -> 
                       let src = a.``Common Name``
                       let ref = findParentRef src parentDict
                       printfn "%s =====> %s (%s)" (parentSite src) (functionalLocation src) ref)


let main () = 
    let csvOutputPath = @"G:\work\Projects\events2\enva\resolved-screen-data.csv"
    let parentDict = getParentRefs ()
    let screens = getImportRows ()
    let writerProc = csvOutput {
        do! tellHeaders csvHeaders
        do! tellRecords screens (fun r -> rowToCsv r parentDict)}
    ignore <| runCsvOutput { Separator = "," } csvOutputPath writerProc   


let rowToJson (row:ImportRow) : JsonOutput<unit> = 
    tellObject <| 
        [ "reference",              tellString <| row.Reference 
        ; "commonName",             tellString <| row.``Common Name`` 
        ; "installedFrom",          tellString <| row.``Installed From``
        ; "manufacturer",           tellString <| row.Manufacturer
        ; "model",                  tellString <| row.Model
        ; "assetStatus",            tellString <| row.AssetStatus
        ; "locationRef",            tellString <| row.``Loc.Ref.``
        ; "screenApertureSize",     tellString <| row.``Screen Aperture Size mm``
        ; "screenType",             tellString <| row.``Screen Type``
        ]

let generateJSON (outputFile:string) : unit = 
    let rows = getImportRows ()
    let proc = tellAsArray rows rowToJson
    ignore <| runJsonOutput {IndentLevel=2} outputFile proc

let main2 () : unit = 
    generateJSON <| @"G:\work\Projects\events2\enva\screen_data.json"
    