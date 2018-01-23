#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
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
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"

#load @"SL\CommonUtils.fs"
#load @"SL\AnswerMonad.fs"
#load @"SL\ScriptMonad.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput

#load @"SL\ExcelUtils.fs"
open SL.ExcelUtils


let outpath = @"G:\work\Projects\ultrasonics\ultrasonic-updates.xlsx"
let headers =  
    [ "Measureand"; "Site name"; "Asset type"
    ; "AssetId"; "Reference"; "Common Name"
    ; "Installed From"; "Manufacturer"
    ; "Tracking"
    ; "Installed From 2"; "Manufacturer 2"; "Specific Model/Frame" ]

type MasterTable = 
    ExcelFile< @"G:\work\Projects\ultrasonics\SLeech-ultrasonics.xlsx",
                SheetName = "Ultrasonics",
                ForceString = true >

type MasterRow = MasterTable.Row

type UpdateTable =
    ExcelFile< @"G:\work\Projects\ultrasonics\AI2-ultrasonics-05.12.17.xlsx",
                SheetName = "Ultrasonics",
                ForceString = true >

type UpdateRow = UpdateTable.Row

let emptyIfNull (input:string) : string =
    match input with
    | null -> ""
    | _ -> input

let compare1 (a:MasterRow) (b:UpdateRow) : int = 
    match (a,b) with
    | (null, _) -> 1
    | (_,null) -> -1
    | _ -> compare a.``Common Name`` b.``Common Name``

let tellHeaders : ClosedXMLOutput<unit> = tellRow <| List.map tellString headers

let processMasterM (x:MasterRow) : ClosedXMLOutput<unit> = 
    match x with
    | null -> closedXMLOutput.Return ()
    | _ ->
        tellRow [ tellString    "Level - Ultrasonic"
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

let processUpdateM (x:UpdateRow) : ClosedXMLOutput<unit> =  
    match x with
    | null -> closedXMLOutput.Return ()
    | _ ->
        tellRow [ tellString        "Level - Ultrasonic"
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

let processMatchM (x:MasterRow) (y:UpdateRow) : ClosedXMLOutput<unit> =  
    match x with
    | null -> printfn "null" ; closedXMLOutput.Return ()
    | _ ->
        if x.``Installed From`` <> y.``Installed From`` then
            tellRow [ tellString    "Level - Ultrasonic"
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
            tellRow [ tellString    "Level - Ultrasonic"
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

//let processMaster (x:MasterRow) : unit = printfn "MASTER: %s" x.``Common Name``
//let processUpdate (x:UpdateRow) : unit = printfn "UPDATE: %s" x.``Common Name``
//let processMatch (x:MasterRow) (y:UpdateRow) : unit = printfn "**Matching: %s" x.``Common Name``
    
let process1 (xs:MasterRow list) (ys:UpdateRow list) : ClosedXMLOutput<unit> = 
    let rec go ms us = 
        match (ms,us) with
        | [], us1 -> mapMz processUpdateM us1
        | ms1, [] -> mapMz processMasterM ms1
        | (m::ms1, u::us1) -> 
            match compare1 m u with
            | x when x < 0 -> closedXMLOutput { do! processMasterM m
                                                do! go ms1 us }
            | x when x = 0 -> closedXMLOutput { do! processMatchM m u
                                                do! go ms1 us1 }
            | x when x > 0 -> closedXMLOutput { do! processUpdateM u
                                                do! go ms us1 }
            | x -> failwith (sprintf "Weird pattern failure: %d" x)
    go xs ys

let main () = 
    let master = new MasterTable()
    let updates = new UpdateTable()
    let action : ClosedXMLOutput<unit> = 
        closedXMLOutput { do! tellHeaders
                          do! process1 (master.Data |> Seq.toList) (updates.Data |> Seq.toList) }
    outputToNew { SheetName = "Ultrasonics" } action outpath 




