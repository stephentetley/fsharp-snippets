#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

open System.IO


type InputTable = 
    ExcelFile< @"G:\work\Projects\cso_pollution\assetlist-for-docgen.xlsx",
               SheetName = "Assets",
               ForceString = true >

type InputRow = InputTable.Row

let outputRoot = @"G:\work\Projects\cso_pollution\output\"
let templateLoc = @"G:\work\Projects\cso_pollution\CSO Combined TEMPLATE.docx"

type SearchList = List<string*string>

let doubleQuote (s:string) : string = "\"" + s + "\""

let refobj (x:'a) : ref<obj> = ref (x :> obj)

let defaultIfNull (dfault:string) (ss:string) : string =
    match ss with
    | null -> dfault
    | _ -> ss

let safeName (input:string) : string = 
    let bads = ['\\'; '/'; ':']
    List.fold (fun s c -> s.Replace(c,'_')) input bads

let maybeCreateDirectory (dirpath:string) : unit = 
    if not <| Directory.Exists(dirpath) then 
        ignore <| Directory.CreateDirectory(dirpath)
    else ()


let replacer (x:Word.Document) (search:string) (replace:string) : bool = 
    let dstart = x.Content.Start
    let dend = x.Content.End
    let rangeall = x.Range(refobj dstart, refobj dend)
    rangeall.Find.ClearFormatting ()
    rangeall.Find.Execute (FindText = refobj search, ReplaceWith = refobj replace)

let replaces (x:Word.Document) (zs:SearchList) : unit = 
    for z in zs do
        match z with | (a,b) -> ignore <| replacer x a b


let process1 (app:Word.Application) (inpath:string) (outpath:string) (ss:SearchList) = 
    let doc = app.Documents.Open(FileName = refobj inpath)
    replaces doc ss
    // This should be wrapped in try...
    try 
        maybeCreateDirectory <| Path.GetDirectoryName outpath
        let outpath1 = doubleQuote outpath
        printfn "Outpath: %s" outpath1
        doc.SaveAs (FileName = refobj outpath1)
    finally 
        doc.Close (SaveChanges = refobj false)


let processName (ss:string) : string = 
    let (parts : string []) = ss.Split('/')
    if parts.Length > 3 then
        let slice = Array.sub parts 2 (parts.Length - 3)
        String.concat "/" slice 
    else 
        "Unknown Process" 

let shortProcessName (ss:string) : string = 
    let (parts : string []) = ss.Split('/')
    if parts.Length > 3 then
        let slice = Array.sub parts (parts.Length - 2) 1
        String.concat " " slice 
    else 
        "Unknown Process" 

let siteName (ss:string) : string = 
    let (parts : string []) = ss.Split('/')
    if parts.Length >= 2 then
        let slice = Array.sub parts 0 2
        String.concat "/" slice 
    else 
        ss

let outfileName (site:string) (prozess:string) = 
    let safe1 = safeName site
    let file = safe1 + " " + safeName prozess + " Survey.docx"
    System.IO.Path.Combine(outputRoot,safe1,file)

let processInputLine (app:Word.Application) (rowi:InputRow) : unit =
    let prozess = processName <| defaultIfNull "Unknown" rowi.``AI2 Asset Loc``
    let shortProzess = shortProcessName <| defaultIfNull "Unknown" rowi.``AI2 Asset Loc``
    let output  = outfileName rowi.``Site Name`` shortProzess
    let searches = 
        [ ("#SITENAME", defaultIfNull "#SITENAME" rowi.``Site Name``)
        ; ("#SAINUMBER", defaultIfNull "#SAINUMBER" rowi.``Site SAI Number ``)
        ; ("#PROCESSNAME", defaultIfNull "" prozess)
        ; ("#PITAG", defaultIfNull "" rowi.``P&I Tag``)  
        ]
    process1 app templateLoc output searches

let main () = 
    let app = new Word.ApplicationClass (Visible = true) 
    let file = new InputTable()
    for (rowi:InputRow) in file.Data do
        match rowi.``Site Name`` with
        | null -> printfn "<nullrow>"
        | _ -> printfn "Processing %s (%s)..." rowi.``Site Name`` rowi.``Asset Id``
               processInputLine app rowi
    app.Quit()

