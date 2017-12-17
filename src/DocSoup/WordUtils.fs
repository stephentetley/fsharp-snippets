[<AutoOpen>]
module DocSoup.WordUtils

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

// Range is a very heavy object to be manipulating start and end points
// Use an alternative...
[<StructuredFormatDisplay("Region: {regionStart} to {regionEnd}")>]
type Region = { regionStart : int; regionEnd : int}


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


let tableRegions(doc:Word.Document) : Region list = 
    let tables : seq<Word.Table> = Seq.cast doc.Tables
    List.map (fun (o:Word.Table) -> extractRegion <| o.Range) <| Seq.toList tables


    
let sectionRegions(doc:Word.Document) : Region list = 
    let sections : seq<Word.Section> = Seq.cast doc.Sections
    List.map (fun (o:Word.Section) -> extractRegion <| o.Range) <| Seq.toList sections
