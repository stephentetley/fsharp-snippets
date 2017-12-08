

#I @"..\..\packages\FParsec.1.0.2\lib\net40-client"
#I @"..\..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FParsec"
#r "FParsecCS"
#r "FSharpx.Collections.dll"

#load @"Syntax.fs"
#load @"FlatSyntax.fs"

open System
open System.IO
open FParsec

open FilePath.Syntax
open FilePath





let test01 () = run FlatSyntax.pMode "-a---"

let test02 () = run FlatSyntax.pTime "16:45"

let test03 () = run FlatSyntax.pDate "07/07/2016"

let test04 () = run FlatSyntax.pName "with spaces.txt\nfalse"

let test05 () = run FlatSyntax.pElement "d-----       27/04/2017     19:44                messy-data"
let test06 () = run FlatSyntax.pElement "-a----       15/05/2017     19:47         305505 HaZaRdS short 3 5 17.xlsx"

let test07 () = run FlatSyntax.pDirectoryName "    Directory: E:\coding\fsharp\fsharp-snippets"

//let test08 () = 
//   let ss = "Mode                LastWriteTime         Length Name          \n\
//            ----                -------------         ------ ----           "
//   in run FlatSyntax.pTitles ss 

let block1 = 
    String.concat "\n" <| 
        [ ""
        ; ""
        ; "    Directory: E:\coding\fsharp\fsharp-snippets"
        ; ""             
        ; ""
        ; "Mode                LastWriteTime         Length Name           "
        ; "----                -------------         ------ ----           "
        ; "d-----       29/05/2017     15:27                ConcatPDFs     "
        ; "d-----       29/05/2017     21:44                PhotoDoc       "
        ; "d-----       30/05/2017     14:55                sketch         "
        ; "d-----       31/05/2017     19:21                WordExtractors "
        ; "d-----       04/06/2017     10:16                _old           "
        ; "-a----       24/06/2017     12:38              0 dir-Recurse.txt "
        ; "-a----       26/03/2017     22:24           1514 LICENSE        "
        ; "-a----       26/03/2017     22:24            125 README.md      "
        ; ""
        ; ""
        ]

let test09 () = run FlatSyntax.pBlock block1

let test10 () = run FlatSyntax.pListing block1

let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-Recurse.txt")

let test11 () = runParserOnFile FlatSyntax.pListing () path1 Text.ASCIIEncoding.ASCII

let test12 () = 
    let opt = FlatSyntax.readListing path1
    match opt with
    | Choice2Of2(a) -> FlatSyntax.topdown a
    | Choice1Of2(s) -> failwith s


let path2 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/chop.txt")
let test13 () = FlatSyntax.readListing path2

let test14 () = ppDate { Day=4; Month=6; Year=2017 }

let test15 () = shortName "E:\\coding\\fsharp\\fsharp-snippets\\WordExtractors\\src\\obj\\Debug"

let test16 () = 
    let opt = FlatSyntax.readListing path1
    match opt with
    | Choice2Of2(a) -> FlatSyntax.topdown a |> 
                      (fun a -> match a with
                                | Some b -> printfn "%s" <| directoryListing b
                                | None -> failwith "Bad")
    | Choice1Of2(s) -> failwith s
