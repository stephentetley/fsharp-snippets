
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

// Run Word...
let app = new Word.ApplicationClass()


let filename = ref (box @"G:\work\working\Survey1.docx")
let doc = app.Documents.Open(filename)

doc.Sections 
    |> Seq.cast<Word.Section> 
    |> Seq.iter (fun s1 -> printfn "Tables: %i" s1.Range.Tables.Count)


doc.Close(SaveChanges = ref (box false))
