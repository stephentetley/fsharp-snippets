// Use FSharp.Data ...

#I @"..\packages\FSharp.Data.2.3.3\lib\net40"
#r @"FSharp.Data.dll"
open FSharp.Data

let rtsIn = CsvFile.Load(@"G:\work\working\rts.csv")

let truncRow0 : System.Func<CsvRow,CsvRow> = 
    System.Func<CsvRow,CsvRow> (fun rowi -> rowi)

let truncRow (rowi : CsvRow) : CsvRow = 
    let cols = Array.map (fun (x : string) -> x.Trim()) rowi.Columns
    new CsvRow(rtsIn, cols)



let out = rtsIn.Map (System.Func<CsvRow,CsvRow>truncRow)

out.Save(path = @"G:\work\working\rts.out.csv", separator=',')
