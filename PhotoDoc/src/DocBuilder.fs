
[<AutoOpen>]
module PhotoDoc.DocBuilder

// Add a reference via the COM tab 
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop


type DocBuilder =
    { RngLast : Word.Range }

