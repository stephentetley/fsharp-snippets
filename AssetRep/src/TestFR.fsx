
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

#load "FindReplace.fs"
open FindReplace


let cleanseName (x:string) = x.Replace("/", "_")

let makeFileName (x:string) : string = 
    let ans = sprintf @"data/%s cover sheet.pdf" (cleanseName x)
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", ans)
    
let makeModList (sitename:string) (sainum:string) : ModList = 
    let ss= [ ("#SAINUM", sainum); ("#SITENAME", sitename) ]
    let outpath = makeFileName sitename
    (outpath, ss)

let sites = [ ("Trafford", "SAI00002351")
            ; ("Walboro", "SAI00002365") ]


let run01 () = 
    let template = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", @"data/TEMPLATE cover sheet.docx")
    let mods = List.map (fun s -> match s with | (x,y) -> makeModList x y)  sites
    processMany template mods

