#I @"C:\Windows\assembly\GAC_MSIL\office\15.0.0.0__71e9bce111e9429c"
#r "office"
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop


open System.IO

#load @"ExcelUtils.fs"
open ExcelUtils

#load "JsonInput.fs"
open JsonInput

// TODO this should use Json for the find/replace pairs

let outputRoot = @"G:\work\Projects\T0975_EDM2\output\"
let templateLoc = @"G:\work\Projects\T0975_EDM2\EDM2 Survey TEMPLATE.docx"
let allSubsitutions = @"G:\work\Projects\T0975_EDM2\site-list-for-GEN.xlsx"
let sheetName = @"SITE_LIST"


type ReplacesList = (string*string) list

let doubleQuote (s:string) : string = "\"" + s + "\""

let refobj (x:'a) : ref<obj> = ref (x :> obj)

let defaultIfNull (dfault:string) (ss:string) : string =
    match ss with
    | null -> dfault
    | _ -> ss

let safeName (input:string) : string = 
    let bads = ['\\'; '/'; ':']
    List.fold (fun s c -> s.Replace(c,'_')) (input.Trim()) bads


let maybeCreateDirectory (dirpath:string) : unit = 
    if not <| Directory.Exists(dirpath) then 
        ignore <| Directory.CreateDirectory(dirpath)
    else ()


let replacer (x:Word.Document) (search:string) (replace:string) : bool = 
    let replaceRange (range1:Word.Range) : bool = 
        range1.Find.ClearFormatting ()
        range1.Find.Execute (FindText = refobj search, 
                                ReplaceWith = refobj replace,
                                Replace = refobj Word.WdReplace.wdReplaceAll)

    let dstart = x.Content.Start
    let dend = x.Content.End
    let ans1 = replaceRange <| x.Range(refobj dstart, refobj dend)
    let header = x.Sections.[1].Headers.[Word.WdHeaderFooterIndex.wdHeaderFooterPrimary].Range
    let ans2 = replaceRange <| header
    ans1 && ans2

let replaces (x:Word.Document) (zs:ReplacesList) : unit = 
    for z in zs do
        match z with | (a,b) -> ignore <| replacer x a b


let process1 (app:Word.Application) (templatePath:string) (outpath:string) (ss:ReplacesList) = 
    let doc = app.Documents.Open(FileName = refobj templatePath)
    replaces doc ss
    // This should be wrapped in try...
    try 
        maybeCreateDirectory <| Path.GetDirectoryName outpath
        let outpath1 = doubleQuote outpath
        printfn "Outpath: %s" outpath1
        doc.SaveAs (FileName = refobj outpath1)
    finally 
        doc.Close (SaveChanges = refobj false)



// new folder for each site...
let outfileName (site:string) = 
    let safe1 = safeName site
    let file = sprintf "%s EDM2 Survey.docx" safe1
    System.IO.Path.Combine(outputRoot,safe1,file)

// Excel rows are Ranges
type TableRow(rng:Excel.Range) = 
    member this.Item
        with get(i) = (rng.Cells.[1,i] :?> Excel.Range).Value2
        and set i value = rng.Cells.[1,i] <- value

let rowValues (row:TableRow) : string list = 
    let rec go ix ac = 
        let h1 = row.[ix]
        match h1 with
        | null -> List.rev ac
        | :? string as s -> go (ix+1) (s::ac)
        | :? int -> go (ix+1) ("N/A"::ac)
        | obj -> let s = (obj.ToString()) in go (ix+1) (s::ac)
    go 1 []

let makeHeaders (worksheet:Excel.Worksheet) : string list = 
    let headers = TableRow(worksheet.Cells.Rows.[1] :?> Excel.Range)
    rowValues headers


// Caution - zipping may truncate
let makeSearches (headers: string list) (row:TableRow) : ReplacesList = 
    List.zip headers <| rowValues row

let processInputLine (app:Word.Application) (headers: string list) (row:TableRow) : unit =
    let sitename = safeName (row.[1] :?> string)
    let outputpath  = outfileName sitename
    let searches = makeSearches headers row
    process1 app templateLoc outputpath searches
    
let replacesDoc (wordApp:Word.ApplicationClass) (outpath:string) (replaces:ReplacesList) : unit =
    process1 wordApp templateLoc outpath replaces

let main () = 
    let wordApp = new Word.ApplicationClass (Visible = true) 
    let excelApp = new Excel.ApplicationClass(Visible = true)
    let workbook : Excel.Workbook = excelApp.Workbooks.Open(allSubsitutions)
    let worksheet = workbook.Sheets.[sheetName] :?> Excel.Worksheet

    let headers = makeHeaders worksheet
    for ix = 1016 to findRowCount worksheet do   // TEMP HACK
    // for ix = 2 to findRowCount worksheet do
        let rowi = TableRow(worksheet.Cells.Rows.[ix] :?> Excel.Range)
        processInputLine wordApp headers rowi
    // Tear down...
    workbook.Close(SaveChanges = false)
    wordApp.Quit()
    excelApp.Quit()

