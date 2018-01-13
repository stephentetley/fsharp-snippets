module SL.CommonUtils

open System



// To go into a common module soon...
let suffixFileName (filePath:string) (suffix:string) = 
    let pathTo = IO.Path.GetDirectoryName filePath
    let ext = IO.Path.GetExtension filePath
    let justname = IO.Path.GetFileNameWithoutExtension filePath
    IO.Path.Combine (pathTo, sprintf "%s%s%s" justname suffix ext)

