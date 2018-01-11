#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open System

#load "CsvOutput.fs"
open CsvOutput

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

let processUpdate (name:string) : CsvOutput<unit> = 
    tellRow [ tellString name; tellString "To make"]

let processMaster (row:MasterRow) : CsvOutput<unit> = 
    csvOutput.Return ()

let processMatch (row:MasterRow) (name:string) : CsvOutput<unit> =
    tellRow [ tellString name; tellString "Exists" ]

let compareElements (row:MasterRow) (name:string) : int = 
    let name1 = noPunctuation <| name.Trim()
    let len = name1.Length
    let prefix = noPunctuation <| (row.Title.Trim()).[0..len-1]
    // printfn "'%s' => '%s'" prefix name1
    compare prefix name1

let processLists (xs:MasterRow list) (ys:string list) : CsvOutput<unit> = 
    let rec go ms us = 
        match (ms,us) with
        | [], us1 -> mapMz processUpdate us1
        | ms1, [] -> mapMz processMaster ms1
        | (m::ms1, u::us1) -> 
            match compareElements m u with
            | x when x < 0 -> csvOutput { do! processMaster m
                                          do! go ms1 us }
            | x when x = 0 -> csvOutput { do! processMatch m u
                                          do! go ms1 us1 }
            | x when x > 0 -> csvOutput { do! processUpdate u
                                          do! go ms us1 }
            | x -> failwith (sprintf "Weird pattern failure: %d" x)
    go xs ys

let main () = 
    let master = buildMaster ()
    let updates = buildListing ()
    let outFile = @"G:\work\Projects\rtu\manuals-TODO.csv"
    let proc = processLists (master |> Seq.toList) (updates |> Seq.toList)
    outputToNew proc outFile ","
