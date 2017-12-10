#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

#load @"Utils.fs"
open WordExtractors.Utils
#load @"DocMonad.fs"
open WordExtractors.DocMonad

// Note to self, this doc is not "well formed". 
// Textual table data is often not split into rows and columns.
let testDoc = @"G:\work\working\Survey1.docx"


let showTable (t1 : Word.Table) = 
    printfn "Rows %i, Columns %i" t1.Rows.Count t1.Columns.Count
    let ans = t1.ConvertToText (rbox Word.WdSeparatorType.wdSeparatorHyphen)
    printfn "Table: %s" ans.Text


let test01 () = 
    let proc (doc:Word.Document) : unit = 
        printfn "Sections: %i" doc.Sections.Count
        printfn "Paragraphs: %i" doc.Paragraphs.Count
        printfn "Tables: %i" doc.Tables.Count
    runOnFileE (lift1 proc) testDoc


let test02 () = 
    let proc (doc:Word.Document) : unit = 
        doc.Tables 
            |> Seq.cast<Word.Table> 
            |> Seq.iter showTable
        doc.Sections 
            |> Seq.cast<Word.Section> 
            |> Seq.iter (fun s1 -> printfn "Tables: %i" s1.Range.Tables.Count)

        let all : Word.Range = doc.Content
        all.Select()
        printfn "Characters: %i" all.Characters.Count
    runOnFileE (lift1 proc) testDoc

let test03 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        printfn "Rows %i, Columns %i" t1.Rows.Count t1.Columns.Count
        printfn "%s" (t1.ConvertToText(rbox Word.WdSeparatorType.wdSeparatorHyphen).Text)
    runOnFileE (lift1 proc) testDoc

let test04 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        let mutable rng1 = t1.Range
        // The mutated range matches what is found.
        let found = rng1.Find.Execute(FindText = rbox "Process Application")
        if found then
            printfn "'%s'" rng1.Text
        else printfn "no found"
    runOnFileE (lift1 proc) testDoc

let textToRightOf(range:Word.Range) (findText:string) : string = 
    let mutable (rng1:Word.Range) = range.Duplicate
    let found = rng1.Find.Execute(FindText = rbox findText)
    if found then
        let reg1 = { regionStart = rng1.End; regionEnd = range.End }
        (trimRange range reg1).Text
    else "Not found"

let test05 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        let s1 = textToRightOf t1.Range "Process Application" 
        printfn "'%s'" s1        
    runOnFileE (lift1 proc) testDoc