
open System.Text.RegularExpressions



let inputFile = @"G:\work\Projects\rtu\AR-asset-expired-2011\structured-text.txt"

/// Regex.Match works only on string input (not StreamReader etc...)
/// Start of 

let temp01 () = 
    let input = System.IO.File.ReadAllText(inputFile)
    let mcol:MatchCollection = Regex.Matches(input, "Body:.*") 
    mcol
        |> Seq.cast<Match> 
        |> Seq.iter (fun m -> printfn "%s" (m.Value.Trim()) )

/// Not regex matches don't give line number.


let temp02 () = 
    let inputLines = System.IO.File.ReadAllLines(inputFile)
    Array.iteri
        (fun ix line -> 
            let mcol = Regex.Matches(line, "^Body:.*") 
            mcol
                |> Seq.cast<Match> 
                |> Seq.iter (fun m -> printfn "%d: %s" (ix+1) (m.Value.Trim()) ))
        inputLines

let summarize1 (matches:string list) = 
    let lineProc (lineNum:int) (line:string) = 
        let match1 (regex:string) = 
            Regex.Matches(line,regex) 
                |> Seq.cast<Match> 
                |> Seq.iter (fun m -> printfn "%d: %s" lineNum (m.Value.Trim()) )
        List.iter match1 matches

    let inputLines = System.IO.File.ReadAllLines(inputFile)
    Array.iteri (fun ix line -> lineProc (ix+1) line) inputLines

/// File.ReadAllInes returns an array - this means we could parse
/// with a cursor and backtracking would be cheap.


let temp03 () = 
    summarize1 
        [ "^Body:.*"
        ; "^RTU Modbus Controller"
        ; "^RTU Modem"
        ; "^RTU Outstation"
        ; "^RTU Power Supply"
        ; "^Product Code.*"
        ; "^Open Channel Flow Transmitter" 
        ; "^RTU Manufacturer.*"
        ; "^RTU Model.*"
        ; "^RTU Type.*"
        ; "^RTU Serial No.*"
        ; "^RTU Serial Number.*"
        ; "^Site.*"
        ]
    
