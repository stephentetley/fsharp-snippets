[<AutoOpen>]
module WordExtractors.DocMonad

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

// TODO should check file exists...
let runOnFile (ma:DocMonad<'a>) (filename:string) : Ans<'a> =
    let app = new Word.ApplicationClass (Visible = true) 
    let doc = app.Documents.Open(FileName = ref (filename :> obj))
    let ans = apply1 ma doc
    doc.Close(SaveChanges = ref (box false))
    app.Quit()
    ans

