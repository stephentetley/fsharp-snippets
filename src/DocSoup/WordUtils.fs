[<AutoOpen>]
module DocSoup.WordUtils

open System.IO
open System.Collections
open System.Collections.Generic

// Add references via the COM tab for Office and Word
// All the PIA stuff online is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop


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
[<StructuredFormatDisplay("Region: {regionStart} to {regionEnd}")>]
type Region = { regionStart : int; regionEnd : int}
    

// Expected to be sorted
type Regions = 
    | Regions of Region list 
    interface IEnumerable<Region> with
        member x.GetEnumerator() = match x with Regions(x) -> (x |> List.toSeq |> Seq.cast<Region>).GetEnumerator()
    
    // Apparently we need to implement theoldschool interface as well
    interface IEnumerable with
        member x.GetEnumerator() = match x with Regions(x) -> (x |> List.toSeq).GetEnumerator() :> IEnumerator


let makeRegions (input:Region list) : Regions = 
    Regions <| List.sortBy (fun o -> o.regionStart) input


let extractRegion (range:Word.Range) : Region = { regionStart = range.Start; regionEnd = range.End }

let trimRange (range:Word.Range) (region:Region) : Word.Range = 
    let mutable r2 = range.Duplicate
    r2.Start <- region.regionStart
    r2.End <- region.regionEnd
    r2


let isSubregionOf (major:Region) (minor:Region) : bool = 
    minor.regionStart >= major.regionStart && minor.regionEnd <= major.regionEnd


let majorLeft (major:Region) (minor:Region) : Region = 
    if major.regionStart <= minor.regionStart then
        { regionStart = major.regionStart; regionEnd = min major.regionEnd minor.regionStart }
    else
        failwith "majorLeft - no region to the left"

let majorRight(major:Region) (minor:Region) : Region = 
    if major.regionEnd >= minor.regionEnd then
        { regionStart = max major.regionStart minor.regionEnd; regionEnd = major.regionEnd }
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
    region1.regionStart <= region2.regionStart

let startsAfter (region1:Region) (region2:Region) : bool = 
    region1.regionStart > region2.regionStart

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
        | [x] -> if x.regionStart > pos then Some x else None
        | (x::xs) -> 
            if x.regionStart > pos then Some x else proc xs
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


     