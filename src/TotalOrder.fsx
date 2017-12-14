#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#load @"ExcelUtils.fs"
open ExcelUtils


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



let compare1 (a:MasterRow) (b:UpdateRow) : int = 
    match (a,b) with
    | (null, _) -> 1
    | (_,null) -> -1
    | _ -> compare a.``Common Name`` b.``Common Name``

let tellHeaders : RowWriter<unit> = tellRow headers

let processMasterM (x:MasterRow) : RowWriter<unit> = 
    match x with
    | null -> rowWriter.Return ()
    | _ ->
        tellRow [ "Level - Ultrasonic"
                ; x.``Site name``
                ; x.``Asset type``
                ; x.AssetId 
                ; x.Reference
                ; x.``Common Name``
                ; x.``Installed From``
                ; x.Manufacturer
                ; "Master (unchanged)"
                ; ""
                ; ""
                ; ""
                ]

let namePart (ix:int) (common:string) = 
    let splits = common.Split [| '/' |]
    if ix < splits.Length then splits.[ix]
    else ""

let processUpdateM (x:UpdateRow) : RowWriter<unit> =  
    match x with
    | null -> rowWriter.Return ()
    | _ ->
        tellRow [ "Level - Ultrasonic"
                ; namePart 0 <| x.``Common Name``
                ; namePart 1 <| x.``Common Name``
                ; x.AssetId
                ; x.Reference
                ; x.``Common Name``
                ; x.``Installed From``
                ; x.Manufacturer
                ; "New on AI2"
                ; ""
                ; ""
                ; x.``Specific Model/Frame``
                ]

let processMatchM (x:MasterRow) (y:UpdateRow) : RowWriter<unit> =  
    match x with
    | null -> printfn "null" ; rowWriter.Return ()
    | _ ->
        if x.``Installed From`` <> y.``Installed From`` then
            tellRow [ "Level - Ultrasonic"
                    ; x.``Site name``
                    ; x.``Asset type``
                    ; x.AssetId 
                    ; x.Reference
                    ; x.``Common Name``
                    ; x.``Installed From``
                    ; x.Manufacturer
                    ; "Master (changed)"
                    ; y.``Installed From``
                    ; y.Manufacturer
                    ; y.``Specific Model/Frame``
                    ]
        else
            tellRow [ "Level - Ultrasonic"
                    ; x.``Site name``
                    ; x.``Asset type``
                    ; x.AssetId 
                    ; x.Reference
                    ; x.``Common Name``
                    ; x.``Installed From``
                    ; x.Manufacturer
                    ; "Master (unchanged)"
                    ; ""
                    ; ""
                    ; ""
                    ]

//let processMaster (x:MasterRow) : unit = printfn "MASTER: %s" x.``Common Name``
//let processUpdate (x:UpdateRow) : unit = printfn "UPDATE: %s" x.``Common Name``
//let processMatch (x:MasterRow) (y:UpdateRow) : unit = printfn "**Matching: %s" x.``Common Name``
    
let process1 (xs:MasterRow list) (ys:UpdateRow list) : RowWriter<unit> = 
    let rec go ms us = 
        match (ms,us) with
        | [], us1 -> iterM processUpdateM us1
        | ms1, [] -> iterM processMasterM ms1
        | (m::ms1, u::us1) -> 
            match compare1 m u with
            | x when x < 0 -> rowWriter { do! processMasterM m
                                          do! go ms1 us }
            | x when x = 0 -> rowWriter { do! processMatchM m u
                                          do! go ms1 us1 }
            | x when x > 0 -> rowWriter { do! processUpdateM u
                                          do! go ms us1 }
            | x -> failwith (sprintf "Weird pattern failure: %d" x)
    go xs ys

let main () = 
    let master = new MasterTable()
    let updates = new UpdateTable()
    let action : RowWriter<unit> = 
        rowWriter { do! tellHeaders
                    do! process1 (master.Data |> Seq.toList) (updates.Data |> Seq.toList) }
    outputToNew action outpath "Ultrasonics"


