#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"

#load @"SL\FileStore.fs"
open SL.FileStore


let test01 () = 
    let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-Recurse.txt")
    match readDirRecurseOutput path1 with
    | Choice1Of2 err -> failwith err
    | Choice2Of2 ans -> printfn "%A" ans
