#I @"C:\Windows\assembly\GAC_MSIL\office\15.0.0.0__71e9bce111e9429c"
#r "office"
#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

open System.IO

#I @"..\packages\FSharp.Data.3.0.0-beta3\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions




// Use Json for the find/replace pairs
// TODO - move to a "skeleton" script configuration so changes
// are localized (see NR's papers on the C-- compiler).
// This is actually more important for the input Json 
// generator - GenFindReplaceInput.fsx


// TODO - outputRoot is a bad idea at this stage.
// The file path for output in the Json input should be fully resolved.
let outputRoot = @"G:\work\Projects\samps\__temp\"
let templateLoc = @"G:\work\Projects\samps\Final_Docs\__Templates\TEMPLATE Samps Cover Sheet.docx"
let allSubsitutions = @"G:\work\Projects\samps\cover-findreplace.json"


type ReplacesList = (string*string) list

type InputConfig = 
    { OutputFileName : string
      FindsReplaces : ReplacesList }



type InputConfigs = InputConfig list




let refobj (x:'a) : ref<obj> = ref (x :> obj)

let doubleQuote (s:string) : string = "\"" + s + "\""

let getNameValuePairs (value:JsonValue) : (string*string) list =
    let fn (k:string, v:JsonValue):(string*string)  = (k, v.AsString() )
    Array.toList <| Array.map fn (value.Properties)

// Structure is known!
// We have a JsonValue object which we can "tree parse".
let extractor (jsonValue:JsonValue) : InputConfigs = 
    let extrObj (value:JsonValue) : InputConfig = 
        { OutputFileName = value.["FileName"].AsString() ;
          FindsReplaces = getNameValuePairs <| value.["Replaces"] }
    [ for v in jsonValue -> extrObj v ]


let readInputs (fileName:string) : InputConfigs = 
    fileName 
        |> File.ReadAllText
        |> JsonValue.Parse 
        |> extractor 


let inputConfigs () : InputConfigs = readInputs allSubsitutions
    
let maybeCreateDirectory (dirpath:string) : unit = 
    if not <| Directory.Exists(dirpath) then 
        ignore <| Directory.CreateDirectory(dirpath)
    else ()

let makeOutputfile (docName:string) : string = 
    System.IO.Path.Combine(outputRoot,docName) 


let getHeadersOrFooters (doc:Word.Document) (proj:Word.Section -> Word.HeadersFooters) : Word.HeaderFooter list = 
    Seq.foldBack (fun (section:Word.Section) (ac:Word.HeaderFooter list) ->
           let headers1 = proj section |> Seq.cast<Word.HeaderFooter>
           Seq.foldBack (fun x xs -> x::xs) headers1 ac)
           (doc.Sections |> Seq.cast<Word.Section>)
           []

let replaceRange (range:Word.Range) (search:string) (replace:string) : unit =
    range.Find.ClearFormatting ()
    ignore <| range.Find.Execute (FindText = refobj search, 
                                    ReplaceWith = refobj replace,
                                    Replace = refobj Word.WdReplace.wdReplaceAll)

let replacer (doc:Word.Document) (search:string, replace:string) : unit =                      
    let rngAll = doc.Range()
    replaceRange rngAll search replace
    let headers = getHeadersOrFooters doc (fun section -> section.Headers)
    let footers = getHeadersOrFooters doc (fun section -> section.Footers)
    List.iter (fun (header:Word.HeaderFooter) -> replaceRange header.Range search replace)
              (headers @ footers)

let replaces (x:Word.Document) (zs:ReplacesList) : unit =
    List.iter (fun sr -> ignore <| replacer x sr) zs


let process1 (app:Word.Application) (templatePath:string) (cfg:InputConfig) : unit = 
    let doc = app.Documents.Open(FileName = refobj templatePath)
    let outpath = makeOutputfile cfg.OutputFileName 
    replaces doc cfg.FindsReplaces
    // This should be wrapped in try...
    try 
        maybeCreateDirectory <| Path.GetDirectoryName outpath
        let outpath1 = doubleQuote outpath
        printfn "Outpath: %s" outpath1
        doc.SaveAs (FileName = refobj outpath1)
    finally 
        doc.Close (SaveChanges = refobj false)


let main () = 
    let wordApp = new Word.ApplicationClass (Visible = true)
    let inputs = inputConfigs ()
    List.iter (process1 wordApp templateLoc) inputs  
    wordApp.Quit()

