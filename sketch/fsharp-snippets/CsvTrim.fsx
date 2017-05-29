// Use FSharp.Data ...

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data


let inpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.csv")

let rtsIn = CsvFile.Load(inpath)


let truncRow (rowi : CsvRow) : CsvRow = 
    let cols = Array.map (fun (x : string) -> x.Trim()) rowi.Columns
    new CsvRow(rtsIn, cols)



let out = rtsIn.Map (System.Func<CsvRow,CsvRow>truncRow)

let outpath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/rts.out.csv")
out.Save(path = outpath, separator=',')
