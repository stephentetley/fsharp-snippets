[<AutoOpen>]
module DocSoup.Base

open System.IO
open System.Collections
open System.Collections.Generic

// Add references via the COM tab for Office and Word
// All the PIA stuff online is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop


// We may want to extract DocMonad to a separate project, so it doesn't depend on ResultMonad
// Replicated here as Ans - this is the error/either monad

type Answer<'a> = 
    | Err of string
    | Ok of 'a

let inline private unitM (x:'a) : Answer<'a> = Ok x


let inline private bindM (ma:Answer<'a>) (f : 'a -> Answer<'b>) : Answer<'b> =
    match ma with
    | Err msg -> Err(msg)
    | Ok a -> f a


type AnswerBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (answerMonad:AnswerBuilder) = new AnswerBuilder()


let fmapM (fn:'a -> 'b) (ans:Answer<'a>) : Answer<'b> = 
    match ans with
    | Err msg -> Err msg
    | Ok a -> Ok (fn a)

let ansMapM (fn:'a -> Answer<'b>) (xs:'a list) : Answer<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> Ok <| List.rev ac
        | z :: zs -> 
            match fn z with
            | Err msg -> Err msg
            | Ok a -> work (a::ac) zs
    work [] xs

let traverseM (fn: 'a -> Answer<'b>) (source:seq<'a>) : Answer<seq<'b>> =
    fmapM (List.toSeq) (ansMapM fn <| Seq.toList source) 


let rbox (v : 'a) : obj ref = ref (box v)



// StringReader appears to be the best way of doing this. 
// Trying to split on a character (or character combo e.g. "\r\n") seems unreliable.
let sRestOfLine (s:string) : string = 
    use reader = new StringReader(s)
    reader.ReadLine ()


// Range is a very heavy object to be manipulating start and end points
// Use an alternative...
[<StructuredFormatDisplay("Region: {RegionStart} to {RegionEnd}")>]
type Region = { RegionStart : int; RegionEnd : int}


let extractRegion (range:Word.Range) : Region = { RegionStart = range.Start; RegionEnd = range.End }
    
let maxRegion (doc:Word.Document) : Region = extractRegion <| doc.Range()


