#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open System


// Just names. ps> dir | select -exp name
let directoryListing = @"G:\work\Projects\rtu\dir.txt"

type MasterTable = 
    ExcelFile< @"G:\work\Projects\rtu\EDMS-docs.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

// Maybe can't implement IComparable for MasterRow as it's a type synonym. 
type MasterRow = MasterTable.Row

let noPunctuation (input:string) : string = 
    let bads = ['\\'; '/'; ':'; '#'; '.'; ','; '_']
    List.fold (fun s c -> s.Replace(c,' ')) input bads


// Sorted lexically on Title
let buildMaster () : seq<MasterRow> = 
    let masterData = new MasterTable()
    let nullPred (row:MasterRow) = match row.Title with null -> false | _ -> true
    let comparison (a:MasterRow) (b:MasterRow):int = compare a.Title b.Title
    masterData.Data 
        |> Seq.filter nullPred
        |> Seq.sortWith comparison

let buildListing () =
    let emptyPred (s:string) = match s.Trim() with "" -> false | _ -> true
    System.IO.File.ReadLines(directoryListing)
        |> Seq.filter emptyPred
        |> Seq.sort

let processUpdate (sw:System.IO.StreamWriter) (name:string) : unit = 
    fprintf sw "%s,To make\n" name

let processMaster (sw:System.IO.StreamWriter) (row:MasterRow) : unit = ()

let processMatch (sw:System.IO.StreamWriter) (row:MasterRow) (name:string) : unit =
    fprintf sw "%s,Exists\n" name

let compareElements (row:MasterRow) (name:string) : int = 
    let name1 = noPunctuation <| name.Trim()
    let len = name1.Length
    let prefix = noPunctuation <| (row.Title.Trim()).[0..len-1]
    // printfn "'%s' => '%s'" prefix name1
    compare prefix name1

let processLists (sw:System.IO.StreamWriter) (xs:MasterRow list) (ys:string list) : unit = 
    let rec go ms us = 
        match (ms,us) with
        | [], us1 -> List.iter (processUpdate sw) us1
        | ms1, [] -> List.iter (processMaster sw) ms1
        | (m::ms1, u::us1) -> 
            match compareElements m u with
            | x when x < 0 -> let _ = processMaster sw m
                              go ms1 us
            | x when x = 0 -> let _ = processMatch sw m u
                              go ms1 us1
            | x when x > 0 -> let _ = processUpdate sw u
                              go ms us1
            | x -> failwith (sprintf "Weird pattern failure: %d" x)
    go xs ys

let main () = 
    let master = buildMaster ()
    let updates = buildListing ()
    use sw = new System.IO.StreamWriter(@"G:\work\Projects\rtu\manuals-TODO.csv")
    processLists sw (master |> Seq.toList) (updates |> Seq.toList)
    sw.Close() 