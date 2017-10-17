[<AutoOpen>]
module FilePath.Syntax

open System.IO
open System.Text
open Microsoft.FSharp.Core

type Name = string

type Date = { Year : int; Month : int; Day : int }

type Time = { Hour :int; Minute : int}

type TimeStamp = TimeStamp of Date * Time

type Mode = string

type FileLength = System.Int64


type File = { Name: Name; Mode: Mode; TimeStamp : TimeStamp; Length: FileLength }

type Directory = { Name: Name; Mode: Mode; TimeStamp : TimeStamp; SubDirs: Directory list; Files: File list }

type Root = { Name: Name; SubDirs: Directory list; Files: File list }


// Note returns empty string if supplied empty string.
let shortName (x:Name) = 
    let parts = x.Split(Path.DirectorySeparatorChar)
    Seq.last parts



// Printing is UK specific dd/mm/yyyy
let ppDate (x:Date) : string = 
    sprintf "%02i/%02i/%04i" x.Day x.Month x.Year

let ppTime (x:Time) : string =
    sprintf "%02i:%02i" x.Hour x.Minute

let ppTimeStamp : (TimeStamp -> string) = 
    function | TimeStamp(d,t) -> sprintf "%s     %s" (ppTime t) (ppDate d)



// Extending StringBuilder is probably the nicest way of making custom Append functions.
type StringBuilder with 
    member this.AppendBlankBlank () =
        ignore <| this.AppendLine ""
        this.AppendLine ""

    member this.AppendHeadings () =
        ignore <| this.AppendLine "Mode                LastWriteTime         Length Name "
        this.AppendLine "----                -------------         ------ ---- "

    member this.AppendDirectoryHeading (x:string) = 
        Printf.bprintf this "    Directory: %s\n" x

    member this.AppendSubDirectory (x:Directory) = 
        Printf.bprintf this "%s       %s               %s\n" x.Mode (ppTimeStamp x.TimeStamp) (shortName x.Name)

    member this.AppendFile (x:File) =
        Printf.bprintf this "%s       %s %14i %s\n" x.Mode (ppTimeStamp x.TimeStamp) x.Length x.Name


let directoryListing1 (x:Root) : StringBuilder = 
    let sb = StringBuilder ()

    ignore <| sb.AppendBlankBlank ()
    sb.AppendDirectoryHeading x.Name
    ignore <| sb.AppendBlankBlank ()
    List.iter sb.AppendSubDirectory x.SubDirs
    List.iter sb.AppendFile x.Files
    sb


let directoryListing (x:Root) : string =
    directoryListing1 x |> (fun sb -> sb.ToString () )