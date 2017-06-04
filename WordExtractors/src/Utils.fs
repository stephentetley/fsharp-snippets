[<AutoOpen>]
module Utils

open System.IO
// Add references via the COM tab for Office and Word
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop


let rbox (v : 'a) : obj ref = ref (box v)

// StringReader appears to be the best way of doing this. 
// Trying to split on a character (or character combo e.g. "\r\n") seems unreliable.
let sRestOfLine (s:string) : string = 
    use reader = new StringReader(s)
    reader.ReadLine ()

let rangeWithin (rbase : Word.Range) (anchor : Word.Range) : bool = 
    anchor.Start >= rbase.Start && anchor.End <= rbase.End

let rightBorder (rbase : Word.Range) (anchor : Word.Range) : int = 
    rbase.End - anchor.End

let leftBorder (rbase : Word.Range) (anchor : Word.Range) : int = 
    anchor.Start - rbase.Start

let rangeRightOf (rbase : Word.Range) (anchor : Word.Range) : Option<Word.Range> = 
    if leftBorder rbase anchor >= 0 && rightBorder rbase anchor > 0 then 
        let newrange = rbase.Duplicate
        newrange.Start <- anchor.End + 1        // plus 1? ...
        Some <| newrange
    else None


let rangeLeftOf (rbase : Word.Range) (anchor : Word.Range) : Option<Word.Range> = 
    if leftBorder rbase anchor > 0 && rightBorder rbase anchor >= 0 then 
        let newrange = rbase.Duplicate
        newrange.End <- anchor.Start - 1        // minus 1? ...
        Some <| newrange
    else None

