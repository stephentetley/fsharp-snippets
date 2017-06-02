﻿
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

let test_rngfind () = let a = rngfind () in printfn "(%i, %i)\n%s" a.Start a.End a.Text

let viewtables () = 
    for table1 in odoc.Tables do 
        printfn "(%i,%i)\n%s" table1.Range.Start table1.Range.End table1.Range.Text

// Have to cast Tables collection to a Seq...
Seq.cast odoc.Tables 
    |> Seq.iter (fun (table1 : Word.Table) -> printfn "(%i,%i)\n" table1.Range.Start table1.Range.End)

// can use end of rng_find to look for next table...


odoc.Close(SaveChanges = rbox false)
oapp.Quit()
