#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\FSharp.Data.2.4.6\lib\net45"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
open System

#load @"SL\AnswerMonad.fs"
#load @"SL\JsonExtractor.fs"
#load @"SL\CsvOutput.fs"
#load @"SL\ExcelProviderHelper.fs"
#load @"SL\ScriptMonad.fs"
open SL.CsvOutput
open SL.ExcelProviderHelper
open SL.ScriptMonad

#load @"Scripts\TotalOrder.fs"
open Scripts.TotalOrder



type MasterTable = 
    ExcelFile< @"G:\work\Projects\rtu\EDMS-docs.xlsx",
                SheetName = "Sheet1",
                ForceString = true >

// Maybe can't implement IComparable for MasterRow as it's a type synonym. 
type MasterRow = MasterTable.Row


let getMasterRows () : MasterRow list = 
    let dict : GetRowsDict<MasterTable, MasterRow> = 
        { GetRows     = fun imports -> imports.Data 
          NotNullProc = fun row -> match row.GetValue(0) with null -> false | _ -> true }
    excelTableGetRows dict (new MasterTable())

// Just names. ps> dir | select -exp name
let directoryListing = @"G:\work\Projects\rtu\dir.txt"

type Script<'a> = ScriptMonad<unit,'a>


let buildListing () =
    let emptyPred (s:string) = match s.Trim() with "" -> false | _ -> true
    System.IO.File.ReadLines(directoryListing)
        |> Seq.filter emptyPred

let processMaster (row:MasterRow) : Script<RowWriter option> =  
    scriptMonad.Return <| None

let processUpdate (name:string) : Script<RowWriter option> = 
    scriptMonad.Return <| Some [ tellString name; tellString "To make"]


let processMatch (row:MasterRow) (name:string) : Script<RowWriter option> = 
    scriptMonad.Return <| Some [ tellString name; tellString "Exists" ]

    
let noPunctuation (input:string) : string = 
    let bads = ['\\'; '/'; ':'; '#'; '.'; ','; '_']
    List.fold (fun s c -> s.Replace(c,' ')) input bads


let compareElements (row:MasterRow) (name:string) : int = 
    let name1 = noPunctuation <| name.Trim()
    let len = name1.Length
    let prefix = noPunctuation <| (row.Title.Trim()).[0..len-1]
    // printfn "'%s' => '%s'" prefix name1
    compare prefix name1

let dictTotalOrderDict : TotalOrderDict<MasterRow,string,unit,RowWriter option> = 
    { CompareLeft = fun a b -> compare a.Title b.Title
      CompareRight = fun a b -> compare a b
      CompareLeftToRight = compareElements
      ProcessLeft = processMaster
      ProcessRight = processUpdate
      ProcessBoth = processMatch }

let csvHeaders = [ "Name"; "Status" ]

let main () = 
    let masterRows = getMasterRows ()
    let updates = buildListing () |> Seq.toList
    let outFile = @"G:\work\Projects\rtu\manuals-To-Make.csv"

    runConsoleScript (printfn "Success: %A") () 
        <| scriptMonad { 
            let! (csvRows:RowWriter list) = 
                fmapM (List.choose id) <| totalOrder dictTotalOrderDict masterRows updates
            do! liftAction (printfn "Generating output...")
            let procCsv = writeRowsWithHeaders csvHeaders csvRows
            do! liftAction (outputToNew {Separator = ","} procCsv outFile)
        }


