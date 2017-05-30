
#r "Microsoft.Office.Interop.Word"

#load "DocBuilder.fs"


open System.IO

let dir1 = @"G:\work\photos1\site photos"


Directory.Exists(dir1)

Directory.EnumerateDirectories(dir1) |> Seq.iter (fun dir -> printfn "%s" dir)

Directory.EnumerateFiles(dir1) |> Seq.iter (printfn "%s")

let rec allFiles dir = 
    seq { for file in Directory.GetFiles dir do
            yield file
          for subdir in Directory.GetDirectories dir do
            yield! allFiles subdir }

allFiles dir1 |> Seq.iter (printfn "%s")