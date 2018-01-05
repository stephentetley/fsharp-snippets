﻿[<AutoOpen>]
module DocSoup.DocMonad

open Microsoft.Office.Interop

// We might extract DocMonad to a separate project so don't rely on ResultMonad
type Ans<'a> = 
    | Err of string
    | Ok of 'a

let mapAns (fn:'a -> 'b) (ans:Ans<'a>) : Ans<'b> = 
    match ans with
    | Err msg -> Err msg
    | Ok a -> Ok (fn a)


// DocMonad is Reader(immutable)+Reader+Error
type DocMonad<'a> = DocMonad of (Word.Document -> Region ->  Ans<'a>)


let inline apply1 (ma : DocMonad<'a>) (doc:Word.Document) (focus:Region) : Ans<'a>= 
    let (DocMonad f) = ma in f doc focus

let private unitM (x:'a) : DocMonad<'a> = DocMonad <| fun _ _ -> Ok x


let bindM (ma:DocMonad<'a>) (f : 'a -> DocMonad<'b>) : DocMonad<'b> =
    DocMonad <| fun doc focus -> 
        match apply1 ma doc focus with
        | Err msg -> Err msg
        | Ok a -> apply1 (f a) doc focus



type DocMonadBuilder() = 
    member self.Return x    = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero ()     = unitM ()

let (docMonad:DocMonadBuilder) = new DocMonadBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:DocMonad<'a>) : DocMonad<'b> = 
    DocMonad <| fun doc focus -> 
        match apply1 ma doc focus with
        | Err msg -> Err msg
        | Ok a -> Ok <| fn a


let liftM (fn:'a -> 'r) (ma:DocMonad<'a>) : DocMonad<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'r> = 
    DocMonad <| fun doc focus -> 
        match apply1 ma doc focus with
        | Err msg -> Err msg
        | Ok a -> 
            match apply1 mb doc focus with 
            | Err msg -> Err msg
            | Ok b -> Ok <| fn a b

let tupleM2 (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb


// DocMonad specific operations
let runOnFile (ma:DocMonad<'a>) (fileName:string) : Ans<'a> =
    if System.IO.File.Exists (fileName) then
        let app = new Word.ApplicationClass (Visible = true) 
        let doc = app.Documents.Open(FileName = ref (fileName :> obj))
        let ans = apply1 ma doc (maxRegion doc)
        doc.Close(SaveChanges = ref (box false))
        app.Quit()
        ans
    else Err <| sprintf "Cannot find file %s" fileName

let runOnFileE (ma:DocMonad<'a>) (fileName:string) : 'a =
    match runOnFile ma fileName with
    | Err msg -> failwith msg
    | Ok a -> a


let throwError (msg:string) : DocMonad<'a> = 
    DocMonad <| fun _ _  -> Err msg

let swapError (msg:string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc focus ->
        match apply1 ma doc focus with
        | Err msg-> Err msg
        | Ok result -> Ok result


let augmentError (fn:string -> string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc focus ->
        match apply1 ma doc focus with
        | Err msg  -> Err <| fn msg
        | Ok result -> Ok result


// Get the text in the currently focused region.
let text : DocMonad<string> = 
    DocMonad <| fun doc focus -> 
        try
            let range = doc.Range(rbox <| focus.RegionStart, rbox <| focus.RegionEnd)
            Ok <| range.Text
        with
        | ex -> Err <| sprintf "text: %s" (ex.ToString())


// Probably should not be part of the API...
let liftGlobalOperation (fn : Word.Document -> 'a) : DocMonad<'a> = 
    DocMonad <| fun doc _ ->
        try
            Ok <| fn doc
        with
        | ex -> Err <| ex.ToString()

// Ideally should be range delimited...
let countTables : DocMonad<int> = 
    liftGlobalOperation <| fun doc -> doc.Tables.Count


// Ideally should be range delimited...
let countSections : DocMonad<int> = 
    liftGlobalOperation <| fun doc -> doc.Sections.Count


// Index starts at 1
//let getTableRegion (index:int) : DocMonad<Region> = 
//    DocMonad <| fun doc pos -> 
//        let table = doc.Tables.[index]
//        Ok (pos, extractRegion table.Range)


//
//let nextTableRegion : DocMonad<Region> = 
//    DocMonad <| fun doc pos -> 
//        let regions = tableRegions doc
//        match findNextAfter regions pos with
//        | None -> Err <| "no next table"
//        | Some(x) -> Ok (x.regionStart, x)
        
