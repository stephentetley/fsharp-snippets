[<AutoOpen>]
module DocSoup.DocMonad

open Microsoft.Office.Interop

type Ans<'a> = 
    | Err of string
    | Ok of 'a

type DocMonad<'a> = DocMonad of (Word.Document -> Ans<'a>)


let inline apply1 (ma : DocMonad<'a>) (doc:Word.Document) : Ans<'a>= 
    let (DocMonad f) = ma in f doc

let unit (x:'a) : DocMonad<'a> = DocMonad (fun r -> Ok x)


let bind (ma:DocMonad<'a>) (f : 'a -> DocMonad<'b>) : DocMonad<'b> =
    DocMonad (fun r -> match apply1 ma r with
                       | Err msg -> Err msg
                       | Ok a -> apply1 (f a) r)



type DocMonadBuilder() = 
    member self.Return x = unit x
    member self.Bind (p,f) = bind p f
    member self.Zero () = unit ()

let (docMonad:DocMonadBuilder) = new DocMonadBuilder()


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

let lift1 (fn : Word.Document -> 'a) : DocMonad<'a> = DocMonad (Ok << fn)

