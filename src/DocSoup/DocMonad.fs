[<AutoOpen>]
module DocSoup.DocMonad

open Microsoft.Office.Interop

type Ans<'a> = 
    | Err of string
    | Ok of 'a

type DocMonad<'a> = DocMonad of (Word.Document -> Ans<'a>)


let inline apply1 (ma : DocMonad<'a>) (doc:Word.Document) : Ans<'a>= 
    let (DocMonad f) = ma in f doc

let private unitM (x:'a) : DocMonad<'a> = DocMonad (fun _ -> Ok x)


let bindM (ma:DocMonad<'a>) (f : 'a -> DocMonad<'b>) : DocMonad<'b> =
    DocMonad <| fun r -> 
        match apply1 ma r with
        | Err msg -> Err msg
        | Ok a -> apply1 (f a) r



type DocMonadBuilder() = 
    member self.Return x    = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero ()     = unitM ()

let (docMonad:DocMonadBuilder) = new DocMonadBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:DocMonad<'a>) : DocMonad<'b> = 
    DocMonad <| fun r -> 
        match apply1 ma r with
        |  Err(msg) -> Err msg
        | Ok(a) -> Ok <| fn a


let liftM (fn:'a -> 'r) (ma:DocMonad<'a>) : DocMonad<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'r> = 
    DocMonad <| fun r -> 
        match apply1 ma r with
        | Err(msg) -> Err msg
        | Ok(a) -> 
            match apply1 mb r with 
            | Err(msg) -> Err msg
            | Ok(b) -> Ok (fn a b)

let tupleM2 (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb


// DocMonad specific operations
let runOnFile (ma:DocMonad<'a>) (fileName:string) : Ans<'a> =
    if System.IO.File.Exists (fileName) then
        let app = new Word.ApplicationClass (Visible = true) 
        let doc = app.Documents.Open(FileName = ref (fileName :> obj))
        let ans = apply1 ma doc
        doc.Close(SaveChanges = ref (box false))
        app.Quit()
        ans
    else Err <| sprintf "Cannot find file %s" fileName

let runOnFileE (ma:DocMonad<'a>) (fileName:string) : 'a =
    match runOnFile ma fileName with
    | Err msg -> failwith msg
    | Ok a -> a


let throwError (msg:string) : DocMonad<'a> = 
    DocMonad <| fun doc -> Err msg

let swapError (msg:string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc ->
        match apply1 ma doc with
        | Err(_) -> Err msg
        | Ok(a) -> Ok a


let augmentError (fn:string -> string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc ->
        match apply1 ma doc with
        | Err(msg) -> Err <| fn msg
        | Ok(a) -> Ok a


let liftOperation (fn : Word.Document -> 'a) : DocMonad<'a> = 
    DocMonad <| fun doc ->
        try
            Ok <| fn doc
        with
        | ex -> Err <| ex.ToString()

let countTables : DocMonad<int> = 
    liftOperation <| fun doc -> doc.Tables.Count

let countSections : DocMonad<int> = 
    liftOperation <| fun doc -> doc.Sections.Count


// Index starts at 1
let getTableRegion (index:int) : DocMonad<Region> = 
    DocMonad <| fun doc -> 
        let table = doc.Tables.[index]
        Ok <| extractRegion table.Range

let getTextInRegion (region:Region) : DocMonad<string> = 
    DocMonad <| fun r -> 
        try
            let range = r.Range(rbox <| region.regionStart, rbox <| region.regionEnd)
            Ok <| range.Text
        with
        | ex -> Err <| sprintf "getTextInRegion: %s" (ex.ToString())
