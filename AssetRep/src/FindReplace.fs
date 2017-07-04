[<AutoOpen>]
module FindReplace

open Microsoft.Office.Interop.Word

type SearchList = List<string*string>

let refobj (x:'a) : ref<obj> = ref (x :> obj)


let replacer (x:Document) (search:string) (replace:string) : bool = 
    let dstart = x.Content.Start
    let dend = x.Content.End
    let rangeall = x.Range(refobj dstart, refobj dend)
    rangeall.Find.ClearFormatting ()
    rangeall.Find.Execute (FindText = refobj search, ReplaceWith = refobj replace)

let replaces (x:Document) (zs:SearchList) : unit = 
    for z in zs do
        match z with | (a,b) -> ignore <| replacer x a b


let process1 (app:Application) (inpath:string) (outpath:string) (ss:SearchList) = 
    let doc = app.Documents.Open(FileName = refobj inpath)
    replaces doc ss
    doc.ExportAsFixedFormat (OutputFileName = outpath, ExportFormat = WdExportFormat.wdExportFormatPDF)
    doc.Close (SaveChanges = refobj false)

type ModList = string * SearchList

let processMany (inpath:string) (mods:ModList list) = 
    let app = new ApplicationClass (Visible = false)
    for m1 in mods do 
        match m1 with | (outpath,ss) ->  process1 app inpath outpath ss
    app.Quit ()
