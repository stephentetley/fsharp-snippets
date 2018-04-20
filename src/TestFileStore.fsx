#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\FParsec.1.0.3\lib\net40-client"
#r "FParsec"

#load @"SL\SwOutput.fs"
#load @"SL\FileStore.fs"
open SL.FileStore


let test01 () = 
    let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-Recurse.txt")
    match readDirRecurseOutput path1 with
    | Choice1Of2 err -> failwith err
    | Choice2Of2 ans -> printfn "%s" <| display ans

let test02 () = 
    let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-no-recurse.txt")
    match readDirRecurseOutput path1 with
    | Choice1Of2 err -> failwith err
    | Choice2Of2 ans -> printfn "%s" <| display ans

let test03 () = 
    let path1 = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/dir-no-recurse.txt")
    match readDirRecurseOutput path1 with
    | Choice1Of2 err -> failwith err
    | Choice2Of2 ans -> printfn "%A" << List.choose id << List.map (tryGetExtension) <| getFiles1 ans


/// TEMP

let testRTU () =
    let path = @"G:\work\projects\rtu\y4-surveys-dir.txt"
    match readDirRecurseOutput path with
    | Choice1Of2 err -> failwith err
    | Choice2Of2 ans -> printfn "%s" <| display ans
