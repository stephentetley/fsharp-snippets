
#r "Microsoft.Office.Interop.Word"

#load "Utils.fs"
#load "Extractors.fs"

open System.IO
open Microsoft.Office.Interop
open Utils
open Extractors

// Note to self - this example is not "properly structured" tables are free text
//
let testpath = @"G:\work\working\Survey1.docx"

let runIt (fn : Word.Document -> 'a) : 'a = 
    let oapp = new Word.ApplicationClass (Visible = true) 
    let odoc = oapp.Documents.Open(FileName = rbox testpath)
    let ans = fn odoc
    odoc.Close(SaveChanges = rbox false)
    oapp.Quit()
    ans


let dummy1 () = 
    let fn (doc : Word.Document) = 
        let dstart = doc.Content.Start
        let dend = doc.Content.End
        doc.Range(rbox dstart, rbox dend).Text
    runIt fn

// Find just finds the sought for text. To be useful for bookmarking
// we would need to see rnage to the right (and maybe to the the left)
let dummy2 () = 
    let rngfind (doc : Word.Document) = 
        let mutable rng = doc.Range()
        let ans = rng.Find.Execute(FindText = rbox "Contractor Information")
        rng.Text
    runIt rngfind



let dummy3 () = 
    let viewtables (doc : Word.Document) = 
        for table1 in doc.Tables do 
            printfn "(%i,%i)\n>>>>>\n%s<<<<<" table1.Range.Start table1.Range.End table1.Range.Text
    runIt viewtables

let dummy3a () = 
    // Have to cast Tables collection to a Seq...
    let action (doc : Word.Document) = 
        Seq.cast doc.Tables 
        |> Seq.iter (fun (table1 : Word.Table) -> printfn "(%i,%i)\n" table1.Range.Start table1.Range.End)
    runIt action




let test1 () = 
    let text = test text testpath
    text

let test2 () = 
    let p1 = parser { let! a = text
                      return a }
    let text = test p1 testpath
    text

let test3 () = 
    let p1 = withTable 1 <| parser { let! a = text
                                     return a }
    let text = test p1 testpath
    text


