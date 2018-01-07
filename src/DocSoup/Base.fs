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


let ansFmap (fn:'a -> 'b) (ans:Answer<'a>) : Answer<'b> = 
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

let ansTraverseM (fn: 'a -> Answer<'b>) (source:seq<'a>) : Answer<seq<'b>> =
    ansFmap (List.toSeq) (ansMapM fn <| Seq.toList source) 


let rbox (v : 'a) : obj ref = ref (box v)



// StringReader appears to be the best way of doing this. 
// Trying to split on a character (or character combo e.g. "\r\n") seems unreliable.
let sRestOfLine (s:string) : string = 
    use reader = new StringReader(s)
    reader.ReadLine ()

// NOTE - Region itself is complicated .
// The semantics of Regions are probably Allen Relations
// A (single point) cursor would be simpler

// Range is a very heavy object to be manipulating start and end points
// Use an alternative...
[<StructuredFormatDisplay("Region: {RegionStart} to {RegionEnd}")>]
type Region = { RegionStart : int; RegionEnd : int}


let extractRegion (range:Word.Range) : Region = { RegionStart = range.Start; RegionEnd = range.End }
    
let maxRegion (doc:Word.Document) : Region = extractRegion <| doc.Range()



///////////////////////////////////////////////////////////////////////
/// Code below may be (largely) obsolete


// Expected to be sorted
type Regions = 
    | Regions of Region list 
    interface IEnumerable<Region> with
        member x.GetEnumerator() = match x with Regions(x) -> (x |> List.toSeq |> Seq.cast<Region>).GetEnumerator()
    
    // Apparently we need to implement theoldschool interface as well
    interface IEnumerable with
        member x.GetEnumerator() = match x with Regions(x) -> (x |> List.toSeq).GetEnumerator() :> IEnumerator


let makeRegions (input:Region list) : Regions = 
    Regions <| List.sortBy (fun o -> o.RegionStart) input



let trimRange (range:Word.Range) (region:Region) : Word.Range = 
    let mutable r2 = range.Duplicate
    r2.Start <- region.RegionStart
    r2.End <- region.RegionEnd
    r2


let isSubregionOf (major:Region) (minor:Region) : bool = 
    minor.RegionStart >= major.RegionStart && minor.RegionEnd <= major.RegionEnd


let majorLeft (major:Region) (minor:Region) : Region = 
    if major.RegionStart <= minor.RegionStart then
        { RegionStart = major.RegionStart; RegionEnd = min major.RegionEnd minor.RegionStart }
    else
        failwith "majorLeft - no region to the left"

let majorRight(major:Region) (minor:Region) : Region = 
    if major.RegionEnd >= minor.RegionEnd then
        { RegionStart = max major.RegionStart minor.RegionEnd; RegionEnd = major.RegionEnd }
    else
        failwith "majorRight - no region to the right"


let rangeToRightOf (range:Word.Range) (findText:string) : option<Word.Range> = 
    let mutable (rng1:Word.Range) = range.Duplicate
    let found = rng1.Find.Execute(FindText = rbox findText)
    if found then
        let reg1 = majorRight (extractRegion range) (extractRegion rng1)
        Some <| trimRange range reg1
    else None


let rangeToLeftOf (range:Word.Range) (findText:string) : option<Word.Range> = 
    let mutable (rng1:Word.Range) = range.Duplicate
    let found = rng1.Find.Execute(FindText = rbox findText)
    if found then
        let reg1 = majorLeft (extractRegion range) (extractRegion rng1)
        Some <| trimRange range reg1
    else None

let rangeBetween (range:Word.Range) (leftText:string) (rightText:string) : option<Word.Range> = 
    let ans1 = rangeToRightOf range leftText
    Option.bind (fun r -> rangeToLeftOf r rightText) ans1


let startsBefore (region1:Region) (region2:Region) : bool = 
    region1.RegionStart <= region2.RegionStart

let startsAfter (region1:Region) (region2:Region) : bool = 
    region1.RegionStart > region2.RegionStart

let tryRegionBeforeTarget (regions:Regions) (target:Region) : Region option = 
    // Want to look at two positions in the list...
    let rec proc rs = 
        match rs with
        | [] -> None
        | [x] -> if startsAfter target x then Some x else None
        | (x1::x2::xs) -> 
            if startsBefore x1 target && startsAfter target x2 then Some x1 else proc (x2::xs)
    proc (Seq.toList regions)

let tryRegionAfterTarget (regions:Regions) (target:Region) : Region option = 
    // Want to look at two positions in the list...
    let rec proc rs = 
        match rs with
        | [] -> None
        | [x] -> if startsAfter target x then Some x else None
        | (x1::x2::xs) -> 
            if startsBefore x1 target && startsAfter target x2 then Some x2 else proc (x2::xs)
    proc (Seq.toList regions)

let findNextAfter (regions:Regions) (pos:int) : Region option = 
    let xs = match regions with | Regions xs -> xs
    let rec proc rs = 
        match rs with
        | [] -> None
        | [x] -> if x.RegionStart > pos then Some x else None
        | (x::xs) -> 
            if x.RegionStart > pos then Some x else proc xs
    proc (match regions with | Regions xs -> xs)


let tableRegions(doc:Word.Document) : Regions = 
    let tables : seq<Word.Table> = doc.Tables |> Seq.cast<Word.Table>
    makeRegions 
        <| List.map (fun (o:Word.Table) -> extractRegion <| o.Range) (Seq.toList tables)


    
let sectionRegions(doc:Word.Document) : Regions = 
    let sections : seq<Word.Section> = doc.Sections |> Seq.cast<Word.Section>
    makeRegions 
        <| List.map (fun (o:Word.Section) -> extractRegion <| o.Range) (Seq.toList sections)

let findText(range:Word.Range) (findText:string) : Region option = 
    let mutable rng1 = range.Duplicate
    let ans:bool = rng1.Find.Execute(FindText = rbox findText)
    if ans then Some <| extractRegion rng1 else None


     