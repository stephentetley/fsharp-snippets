module SL.CsvUtils


open FSharp.Data

open SL.CsvOutput


type CsvTrimOptions = 
    { InputSeparator: string
      InputHasHeaders: bool
      OutputSeparator: string }

let trimCsvFile (options:CsvTrimOptions) (inputFile:string) (outputFile:string) : unit =
    let truncRow (row:CsvRow) : SL.CsvOutput.CellWriter list = 
        Array.foldBack (fun (value:string) ac -> 
                         let a = value.Trim() |> SL.CsvOutput.tellString in a::ac) row.Columns [] 
        
    let csvRows : seq<CsvRow> = 
        CsvFile.Load(uri=inputFile, 
            separators = options.InputSeparator,
            hasHeaders = options.InputHasHeaders, 
            quote= '"' ).Rows

    let procM : CsvOutput<unit> = 
        SL.CsvOutput.traverseMz (SL.CsvOutput.tellRow << truncRow) csvRows
        
    SL.CsvOutput.outputToNew {Separator=options.OutputSeparator} procM outputFile    

// Trying to use CsvProvider to to write "untyped" output has proved
// to be fruitlessly complex. Our own CsvOutput module is fine.