#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

let withDoc (f:Word.Document -> 'a) (filename:string) : 'a = 
    let app = new Word.ApplicationClass (Visible = true) 
    let doc = app.Documents.Open(FileName = ref (filename :> obj))
    let ans = f doc
    doc.Close(SaveChanges = ref (box false))
    app.Quit()
    ans


let test01 () = 
    let proc (doc:Word.Document) : unit = 
        printfn "Sections: %i" doc.Sections.Count
        printfn "Paragraphs: %i" doc.Paragraphs.Count
        printfn "Tables: %i" doc.Tables.Count
    withDoc proc @"G:\work\working\Survey1.docx"


let showTable (t1 : Word.Table) = 
    printfn "Rows %i, Columns %i" t1.Rows.Count t1.Columns.Count
    let ans = t1.ConvertToText(ref (box Word.WdSeparatorType.wdSeparatorHyphen))
    printfn "Table: %s" ans.Text



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
    withDoc proc @"G:\work\working\Survey1.docx"

