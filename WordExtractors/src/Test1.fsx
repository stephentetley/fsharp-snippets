
#r "Microsoft.Office.Interop.Word"

#load "ExtractorCombinators.fs"

open System.IO
open Microsoft.Office.Interop
open ExtractorCombinators


let testpath = @"G:\work\working\Survey1.docx"

let oapp = new Word.ApplicationClass (Visible = true) 
let odoc = oapp.Documents.Open(FileName = rbox testpath)

let dstart = odoc.Content.Start
let dend = odoc.Content.End

let rngtext () = odoc.Range(rbox dstart, rbox dend).Text

let rngfind () = 
    let mutable rng = odoc.Range(rbox dstart, rbox dend)
    let ans = rng.Find.Execute(FindText = rbox "Contractor Information")
    rng

let test1 = let a = rngfind () in printfn "(%i, %i)\n%s" a.Start a.End a.Text
