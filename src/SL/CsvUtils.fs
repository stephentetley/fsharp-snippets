module SL.CsvUtils


open FSharp.Data

open SL.CsvOutput


type CsvTrimOptions = 
    { InputSeparator: string
      InputHasHeaders: bool
      OutputSeparator: string }

/// This one uses SL.CsvOutput.
let trimCsvFile (options:CsvTrimOptions) (inputFile:string) (outputFile:string) : unit =
    let truncRow (row:CsvRow) : RowWriter = 
        row.Columns 
            |> Array.toList
            |> List.map (fun (s:string) -> tellString (s.Trim()))


    let csvInput : CsvFile = 
        CsvFile.Load(uri=inputFile, 
            separators = options.InputSeparator,
            hasHeaders = options.InputHasHeaders, 
            quote= '"' )
            
    let headers : option<string list> = 
        csvInput.Headers |> Option.map (Array.toList)

    let procM : CsvOutput<unit> = 
        match headers with
        | None -> tellRecords csvInput.Rows truncRow 
        | Some titles -> 
            writeRecordsWithHeaders titles csvInput.Rows truncRow
        
    SL.CsvOutput.outputToNew {Separator=options.OutputSeparator} procM outputFile    


let private optQuote(s:string) : string = 
    if String.length s > 0 && s.[0] <> '"' then 
        if s.Contains "," then
            sprintf "\"%s\"" s
        else
            s
    else
        s


/// This one writes directly to a StreamWriter.
let trimCsvFile2 (options:CsvTrimOptions) (inputFile:string) (outputFile:string) : unit =
    let rowToTrimmedString (row:string []) : string = 
        let sep = options.OutputSeparator
        String.concat sep <| Array.map (fun (s:string) -> optQuote <| s.Trim()) row
        
    let csvInput : CsvFile = 
        CsvFile.Load(uri=inputFile, 
            separators = options.InputSeparator,
            hasHeaders = options.InputHasHeaders, 
            quote= '"' )
   
    use sw = new System.IO.StreamWriter(outputFile)
   
    match csvInput.Headers with
    | Some titles -> 
        rowToTrimmedString titles |> sw.WriteLine
    | None -> ()
    
    Seq.iter (fun (row:CsvRow) -> 
                row.Columns |> rowToTrimmedString |> sw.WriteLine) csvInput.Rows 
    

// Trying to use CsvProvider to to write "untyped" output has proved to be 
// fruitlessly complex. 
// Our own CsvOutput module or outputting lines to a StreamWriter is fine.