
open System.IO
open System.Text.RegularExpressions

let InputPath = @"G:\work\pdfs-temp\Example1"
let OutputName = @"Example1 Test.pdf"
let GSPath = @"C:\programs\gs\gs9.15\bin\gswin64c.exe"

let Options = @"-dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress"

let doubleQuote s = "\"" + s + "\""

let rec allFiles dir = 
    seq { for file in Directory.GetFiles dir do
            yield file
          for subdir in Directory.GetDirectories dir do
            yield! allFiles subdir }

let filterPDFs (ss:string seq) = 
    let re = new Regex("\.pdf$")
    Seq.filter (fun s -> re.Match(s).Success) ss


let outputOption (name:string) = "-sOutputFile=" + doubleQuote name

let pdfList () = allFiles InputPath |> filterPDFs |> Seq.iter (printfn "%s")

let makeCmd () = 
    let line1 = String.concat " " [ doubleQuote GSPath; Options; outputOption OutputName ]
    let linesK = allFiles InputPath |> filterPDFs |> Seq.map doubleQuote
    String.concat "^\n" (line1 :: Seq.toList linesK)


