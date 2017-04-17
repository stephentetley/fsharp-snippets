
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

// Run Word...
let app = new Word.ApplicationClass ()
let filename = ref (box @"G:\work\working\Survey1.docx")
let doc = app.Documents.Open(filename)


printfn "Sections: %i" doc.Sections.Count
printfn "Paragraphs: %i" doc.Paragraphs.Count
printfn "Tables: %i" doc.Tables.Count


let showTable (t1 : Word.Table) = 
    let ans = t1.ConvertToText(ref (box Word.WdSeparatorType.wdSeparatorHyphen))
    printfn "Table: %s" ans.Text

let add1 i = i + 1

doc.Tables 
    |> Seq.cast<Word.Table> 
    |> Seq.iter showTable


doc.Sections 
    |> Seq.cast<Word.Section> 
    |> Seq.iter (fun s1 -> printfn "Tables: %i" s1.Range.Tables.Count)

let all : Word.Range = doc.Content
all.Select()
printfn "Characters: %i" all.Characters.Count

doc.Close(SaveChanges = ref (box false))
app.Quit()
