[<AutoOpen>]
module WordExtractors.Utils

open System.IO

// Add references via the COM tab for Office and Word
// All the PIA stuff online is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop


let rbox (v : 'a) : obj ref = ref (box v)

// StringReader appears to be the best way of doing this. 
// Trying to split on a character (or character combo e.g. "\r\n") seems unreliable.
let sRestOfLine (s:string) : string = 
    use reader = new StringReader(s)
    reader.ReadLine ()



