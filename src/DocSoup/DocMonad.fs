[<AutoOpen>]
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

type Cursor = int

// DocMonad is State+Reader+Error
type DocMonad<'a> = DocMonad of (Word.Document -> Cursor ->  Ans<Cursor * 'a>)


let inline apply1 (ma : DocMonad<'a>) (doc:Word.Document) (pos:Cursor) : Ans<Cursor * 'a>= 
    let (DocMonad f) = ma in f doc pos

let private unitM (x:'a) : DocMonad<'a> = DocMonad <| fun _ pos -> Ok (pos,x)


let bindM (ma:DocMonad<'a>) (f : 'a -> DocMonad<'b>) : DocMonad<'b> =
    DocMonad <| fun doc pos -> 
        match apply1 ma doc pos with
        | Err msg -> Err msg
        | Ok (pos1,a) -> apply1 (f a) doc pos1



type DocMonadBuilder() = 
    member self.Return x    = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero ()     = unitM ()

let (docMonad:DocMonadBuilder) = new DocMonadBuilder()

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:DocMonad<'a>) : DocMonad<'b> = 
    DocMonad <| fun doc pos -> 
        match apply1 ma doc pos with
        | Err(msg) -> Err msg
        | Ok(pos1,a) -> Ok (pos1, fn a)


let liftM (fn:'a -> 'r) (ma:DocMonad<'a>) : DocMonad<'r> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'r) (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'r> = 
    DocMonad <| fun doc pos -> 
        match apply1 ma doc pos with
        | Err(msg) -> Err msg
        | Ok(pos1,a) -> 
            match apply1 mb doc pos1 with 
            | Err(msg) -> Err msg
            | Ok(pos2,b) -> Ok (pos2, fn a b)

let tupleM2 (ma:DocMonad<'a>) (mb:DocMonad<'b>) : DocMonad<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb


// DocMonad specific operations
let runOnFile (ma:DocMonad<'a>) (fileName:string) : Ans<'a> =
    if System.IO.File.Exists (fileName) then
        let app = new Word.ApplicationClass (Visible = true) 
        let doc = app.Documents.Open(FileName = ref (fileName :> obj))
        let ans = mapAns snd <| apply1 ma doc 0
        doc.Close(SaveChanges = ref (box false))
        app.Quit()
        ans
    else Err <| sprintf "Cannot find file %s" fileName

let runOnFileE (ma:DocMonad<'a>) (fileName:string) : 'a =
    match runOnFile ma fileName with
    | Err msg -> failwith msg
    | Ok a -> a


let throwError (msg:string) : DocMonad<'a> = 
    DocMonad <| fun doc pos -> Err msg

let swapError (msg:string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc pos ->
        match apply1 ma doc pos with
        | Err msg-> Err msg
        | Ok result -> Ok result


let augmentError (fn:string -> string) (ma:DocMonad<'a>) : DocMonad<'a> = 
    DocMonad <| fun doc pos ->
        match apply1 ma doc pos with
        | Err msg  -> Err <| fn msg
        | Ok result -> Ok result


let liftOperation (fn : Word.Document -> 'a) : DocMonad<'a> = 
    DocMonad <| fun doc pos ->
        try
            Ok <| (pos, fn doc)
        with
        | ex -> Err <| ex.ToString()

let countTables : DocMonad<int> = 
    liftOperation <| fun doc -> doc.Tables.Count

let countSections : DocMonad<int> = 
    liftOperation <| fun doc -> doc.Sections.Count


// Index starts at 1
let getTableRegion (index:int) : DocMonad<Region> = 
    DocMonad <| fun doc pos -> 
        let table = doc.Tables.[index]
        Ok (pos, extractRegion table.Range)

let getTextInRegion (region:Region) : DocMonad<string> = 
    DocMonad <| fun doc pos -> 
        try
            let range = doc.Range(rbox <| region.regionStart, rbox <| region.regionEnd)
            Ok (pos, range.Text)
        with
        | ex -> Err <| sprintf "getTextInRegion: %s" (ex.ToString())
