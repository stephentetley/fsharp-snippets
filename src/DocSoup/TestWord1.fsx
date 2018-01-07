#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Word\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Word"
open Microsoft.Office.Interop

#load @"Base.fs"
open DocSoup.Base
#load @"DocMonad.fs"
open DocSoup.DocMonad

// Note to self, this test doc is not "well formed". 
// Textual table data is often not split into rows and columns.
let testDoc = @"G:\work\working\Survey1.docx"


let showTable (t1 : Word.Table) = 
    printfn "Rows %i, Columns %i" t1.Rows.Count t1.Columns.Count
    let ans = t1.ConvertToText (rbox Word.WdSeparatorType.wdSeparatorHyphen)
    printfn "Table: %s" ans.Text


let test01 () = 
    let proc (doc:Word.Document) : unit = 
        printfn "Sections: %i" doc.Sections.Count
        printfn "Paragraphs: %i" doc.Paragraphs.Count
        printfn "Tables: %i" doc.Tables.Count
    runOnFileE (liftGlobalOperation proc) testDoc


let test02 () = 
    let proc (doc:Word.Document) : unit = 
        doc.Tables 
            |> Seq.cast<Word.Table> 
            |> Seq.iter showTable
        doc.Sections 
            |> Seq.cast<Word.Section> 
            |> Seq.iter (fun s1 -> printfn "Tables: %i" s1.Range.Tables.Count)

        let all : Word.Range = doc.Content
        all.Select()
        printfn "Characters: %i" all.Characters.Count
    runOnFileE (liftGlobalOperation proc) testDoc

let test03 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        printfn "Rows %i, Columns %i" t1.Rows.Count t1.Columns.Count
        printfn "%s" (t1.ConvertToText(rbox Word.WdSeparatorType.wdSeparatorHyphen).Text)
    runOnFileE (liftGlobalOperation proc) testDoc

let test04 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        let mutable rng1 = t1.Range
        // The mutated range matches what is found.
        let found = rng1.Find.Execute(FindText = rbox "Process Application")
        if found then
            printfn "'%s'" rng1.Text
        else printfn "no found"
    runOnFileE (liftGlobalOperation proc) testDoc


// To do should allow wildcards...
let test05 () = 
    let proc (doc:Word.Document) : unit = 
        let t1 = doc.Tables.[1]
        let s1 = Option.map (fun (r:Word.Range) -> r.Text) (rangeToLeftOf t1.Range "Process Application")
        printfn "'%s'\n----------" (if s1.IsSome then Option.get s1 else "<none>")
        let s2 = Option.map (fun (r:Word.Range) -> r.Text) (rangeToRightOf t1.Range "Process Application")
        printfn "'%s'\n----------" (if s2.IsSome then Option.get s2 else "<none>")
        let s3 = Option.map (fun (r:Word.Range) -> r.Text.Trim()) (rangeBetween t1.Range "Process Application" "Site Area")
        printfn "'%s'\n----------" (if s3.IsSome then Option.get s3 else "<none>")

    runOnFileE (liftGlobalOperation proc) testDoc

let test06 () = 
    let proc (doc:Word.Document) : unit = 
        printfn "** Sections"
        Seq.iter (printfn "%A") <| sectionRegions doc
        printfn "** Tables"
        Seq.iter (printfn "%A") <| tableRegions doc
//        printfn "** Match 'Site'"
//        Seq.iter(printfn "%A") <| findallText (doc.Range()) "Site"
    runOnFileE (liftGlobalOperation proc) testDoc

let test07 () = 
    let proc (doc:Word.Document) : unit = 
        let table1 = doc.Tables.[1]
        printfn ">>>\n%s\n<<<" (table1.Range.Text)
        let region1 = extractRegion <| table1.Range
        printfn ">>>\n%s\n<<<" (trimRange (doc.Range()) region1).Text
    runOnFileE (liftGlobalOperation proc) testDoc

let test08 () = 
    let proc = tupleM2 countTables countSections
    printfn "%A" <| runOnFileE proc testDoc

// All text of the document
let test09 () = 
    printfn "%A" <| runOnFileE text testDoc

// This is nice and high level...
let test10 () = 
    let proc = docMonad { 
        let! a0 = table 1 <| cell (0,0) text
        let! c0 = table 3 <| cell (0,0) text
        let! c1 = table 3 <| cell (1,0) text
        let! c2 = table 3 <| cell (2,0) text
        let! c3 = table 3 <| cell (3,0) text
        let! c4 = table 3 <| cell (4,0) text
        let! c5 = table 3 <| cell (5,0) text
        let! c6 = table 3 << cell (6,0) <| text
        return [a0;c0;c1;c2;c3;c4;c5]
    }
    printfn "%A" <| runOnFileE proc testDoc

let shorten (s:string) = if s.Length > 10 then s.[0..9]+"..." else s

let test11 () = 
    let proc = docMonad { 
        let! i = countTables
        let! xs = mapTablesWith (fmapM shorten text)
        return (i,xs)
    }
    printfn "%A" <| runOnFileE proc testDoc

let test12 () = 
    let proc = docMonad { 
        let! (i,xs) = table 3 <| tupleM2 (countCells) (mapCellsWith (fmapM shorten text))
        return (i,xs)
    }
    printfn "%A" <| runOnFileE proc testDoc
