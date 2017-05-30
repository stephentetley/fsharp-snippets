
[<AutoOpen>]
module PhotoDoc.DocBuilder

// Add a reference via the COM tab 
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop

open System

// This is implemented as an object but actually a Writer monad-like API might be better


type DocBuilder =
    val mutable private rnglast : Word.Range
    
    member private x.GotoEnd () = 
        match x.rnglast with
        | null -> ()
        | _ -> x.rnglast.InsertParagraphAfter ()
               let parent = x.rnglast.Document
               let pcount = parent.Paragraphs.Count 
               x.rnglast <- parent.Paragraphs.Item(pcount).Range
               
               
                

    member public x.Document  with get () : Word.Document = x.rnglast.Document

    member public x.AppendPicture (filename : string) = 
        match x.rnglast with
        | null -> ()
        | _ -> x.GotoEnd ()
               ignore <| x.rnglast.InlineShapes.AddPicture(FileName = filename)

    
    new (odoc : Word.Document) = 
        let ix = odoc.Paragraphs.Count
        let r1 : Word.Range = if ix > 0 then odoc.Paragraphs.Item(ix).Range
                              else odoc.Range(Start = ref (0 :> obj)) 
        { rnglast = r1 }
        






    



