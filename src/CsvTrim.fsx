// Use FSharp.Data ...

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data


let inpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.csv")
let outpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.out.csv")
let truncRow (parent:CsvFile) (rowi:CsvRow) : CsvRow = 
    let cols = Array.map (fun (x : string) -> x.Trim()) rowi.Columns
    new CsvRow(parent, cols)


let test01 () = 
    let rtsIn = CsvFile.Load(inpath)
    let tr = fun row -> truncRow rtsIn row
    let out = rtsIn.Map  (System.Func<CsvRow,CsvRow>tr)
    out.Save(path = outpath, separator=',')

