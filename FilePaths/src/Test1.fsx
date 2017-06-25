

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#load "Syntax.fs"
#load "FlatSyntax.fs"

open System
open FParsec
open FilePath.Syntax
open FilePath.FlatSyntax





let test01 () = run pMode "-a---"

let test02 () = run pTime "16:45"

let test03 () = run pDate "07/07/2016"

let test04 () = run pName "with spaces.txt\nfalse"

let test05 () = run pElement "d-----       27/04/2017     19:44                messy-data"
let test06 () = run pElement "-a----       15/05/2017     19:47         305505 HaZaRdS short 3 5 17.xlsx"

let test07 () = run pDirectoryName "    Directory: E:\coding\fsharp\fsharp-snippets"

let test08 () = 
   let ss = "Mode                LastWriteTime         Length Name          \n\
            ----                -------------         ------ ----           "
   in run pTitles ss 

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

let test09 () = run pBlock block1

let test10 () = run pListing block1

let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-Recurse.txt")

let test11 () = runParserOnFile pListing () path1 Text.ASCIIEncoding.ASCII


