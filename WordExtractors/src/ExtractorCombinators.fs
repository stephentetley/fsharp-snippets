[<AutoOpen>]
module ExtractorCombinators

// Add a reference via the COM tab 
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop



let rbox (v : 'a) : obj ref = ref (box v)

type Input = Word.Document

    // Note from a Range we can get back to Doc

    // Use "find" to get the first locus