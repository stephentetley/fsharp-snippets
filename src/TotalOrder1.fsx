#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"


#load @"SL\CommonUtils.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\AnswerMonad.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ScriptMonad.fs"
open SL.ExcelProviderHelper
open SL.CsvOutput
open SL.ScriptMonad

#load @"Scripts\TotalOrder.fs"
open Scripts.TotalOrder



type MasterTable = 
    ExcelFile< @"G:\work\Projects\usar\sl-master.xlsx",
                SheetName = "Instrs",
                ForceString = true >

type MasterRow = MasterTable.Row

let getMasterRows () : MasterRow list = 
    let dict : GetRowsDict<MasterTable, MasterRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new MasterTable())


type UpdateTable =
    ExcelFile< @"G:\work\Projects\usar\ADB-instrs-05.12.17.xlsx",
                SheetName = "Ultrasonics",
                ForceString = true >

type UpdateRow = UpdateTable.Row

let getUpdateRows () : UpdateRow list = 
    let dict : GetRowsDict<UpdateTable, UpdateRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new UpdateTable())

type Script<'a> = ScriptMonad<unit,'a>



let emptyIfNull (input:string) : string =
    match input with
    | null -> ""
    | _ -> input

let compareLeftRight (a:MasterRow) (b:UpdateRow) : int = 
    match (a,b) with
    | (null, _) -> 1
    | (_,null) -> -1
    | _ -> compare a.``Common Name`` b.``Common Name``


let processMasterM (x:MasterRow) : Script<RowWriter> = 
    match x with
    | null -> scriptMonad.Return []         // should never match
    | _ ->
        scriptMonad.Return 
            <|  [ tellString    "Level - Ultrasonic"
                ; tellString    <| emptyIfNull x.``Site name``
                ; tellString    <| emptyIfNull x.``Asset type``
                ; tellString    <| emptyIfNull x.AssetId 
                ; tellString    <| emptyIfNull x.Reference
                ; tellString    <| emptyIfNull x.``Common Name``
                ; tellString    <| emptyIfNull x.``Installed From``
                ; tellString    <| emptyIfNull x.Manufacturer
                ; tellString    "Master (unchanged)"
                ; tellString    ""
                ; tellString    ""
                ; tellString    ""
                ]

let namePart (ix:int) (common:string) : string = 
    match common with 
    | null -> ""
    | _ -> 
        let splits = common.Split [| '/' |]
        if ix < splits.Length then splits.[ix]
        else ""

let processUpdateM (x:UpdateRow) : Script<RowWriter> =  
    match x with
    | null -> scriptMonad.Return []
    | _ ->
        scriptMonad.Return
            <|  [ tellString        "Level - Ultrasonic"
                ; tellString        <| namePart 0 (x.``Common Name``)
                ; tellString        <| namePart 1 (x.``Common Name``)
                ; tellString        <| emptyIfNull x.AssetId
                ; tellString        <| emptyIfNull x.Reference
                ; tellString        <| emptyIfNull x.``Common Name``
                ; tellString        <| emptyIfNull x.``Installed From``
                ; tellString        <| emptyIfNull x.Manufacturer
                ; tellString        "New on AI2"
                ; tellString        ""
                ; tellString        ""
                ; tellString        <| emptyIfNull x.``Specific Model/Frame``
                ]

let processMatchM (x:MasterRow) (y:UpdateRow) : Script<RowWriter> =  
    match x with
    | null -> printfn "null" ; scriptMonad.Return []
    | _ ->
        if x.``Installed From`` <> y.``Installed From`` then
            scriptMonad.Return 
                <|  [ tellString    "Level - Ultrasonic"
                    ; tellString    <| emptyIfNull x.``Site name``
                    ; tellString    <| emptyIfNull x.``Asset type``
                    ; tellString    <| emptyIfNull x.AssetId 
                    ; tellString    <| emptyIfNull x.Reference
                    ; tellString    <| emptyIfNull x.``Common Name``
                    ; tellString    <| emptyIfNull x.``Installed From``
                    ; tellString    <| emptyIfNull x.Manufacturer
                    ; tellString    "Master (changed)"
                    ; tellString    <| emptyIfNull y.``Installed From``
                    ; tellString    <| emptyIfNull y.Manufacturer
                    ; tellString    <| emptyIfNull y.``Specific Model/Frame``
                    ]
        else
            scriptMonad.Return  
                <|  [ tellString    "Level - Ultrasonic"
                    ; tellString    <| emptyIfNull x.``Site name``
                    ; tellString    <| emptyIfNull x.``Asset type``
                    ; tellString    <| emptyIfNull x.AssetId 
                    ; tellString    <| emptyIfNull x.Reference
                    ; tellString    <| emptyIfNull x.``Common Name``
                    ; tellString    <| emptyIfNull x.``Installed From``
                    ; tellString    <| emptyIfNull x.Manufacturer
                    ; tellString    "Master (unchanged)"
                    ; tellString    ""
                    ; tellString    ""
                    ; tellString    <| ""
                    ]


let csvHeaders =  
    [ "Measureand"; "Site name"; "Asset type"
    ; "AssetId"; "Reference"; "Common Name"
    ; "Installed From"; "Manufacturer"
    ; "Tracking"
    ; "Installed From 2"; "Manufacturer 2"; "Specific Model/Frame" ]

let dictTotalOrderDict: TotalOrderDict<MasterRow,UpdateRow,unit,RowWriter> = 
    { CompareLeft = fun a b -> compare a.``Common Name`` b.``Common Name``
      CompareRight = fun a b -> compare a.``Common Name`` b.``Common Name``
      CompareLeftToRight = compareLeftRight
      ProcessLeft = processMasterM
      ProcessRight = processUpdateM
      ProcessBoth = processMatchM }


let main () = 
    let masterRows = getMasterRows ()
    let updateRows = getUpdateRows ()
    let outFile = @"G:\work\Projects\usar\usar-updates-total-order.csv"
    runConsoleScript (printfn "Success: %A") () 
        <| scriptMonad { 
            let! (csvRows:RowWriter list) = totalOrder dictTotalOrderDict masterRows updateRows
            do! liftAction (printfn "Generating output...")
            let procCsv = writeRowsWithHeaders csvHeaders csvRows
            do! liftAction (outputToNew {Separator = ","} procCsv outFile)
        }





