
#r "Microsoft.Office.Interop.Word"

#load "DocBuilder.fs"


open System.IO
open Microsoft.Office.Interop
open PhotoDoc.DocBuilder


let dir1 = @"G:\work\photos1\site photos"
let docname = @"G:\work\photos1.doc"

Directory.Exists(dir1)

Directory.EnumerateDirectories(dir1) |> Seq.iter (fun dir -> printfn "%s" dir)

Directory.EnumerateFiles(dir1) |> Seq.iter (printfn "%s")

let rec allFiles dir = 
    seq { for file in Directory.GetFiles dir do
            yield file
          for subdir in Directory.GetDirectories dir do
            yield! allFiles subdir }

allFiles dir1 |> Seq.iter (printfn "%s")

let rbox v = ref (box v)


let TestIt () = 
    let oapp = new Word.ApplicationClass (Visible = true) 
    let odoc = oapp.Documents.Add()
    let obuild = new DocBuilder(odoc)
    allFiles dir1 |> Seq.iter (obuild.AppendPicture)
    odoc.SaveAs(FileName = rbox docname)
    odoc.Close(SaveChanges = rbox false)
    oapp.Quit()



