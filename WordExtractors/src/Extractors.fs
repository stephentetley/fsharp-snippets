[<AutoOpen>]
module Extractors

    // Add a reference via the COM tab 
    // All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
    open Microsoft.Office.Interop
    open Utils



    type Result<'a> = 
        | Okay of 'a
        | Fail of string


    type WDoc = Word.Document

    // TODO - what object is best to store as a cursor - a range or an integer position?
    // We should be very careful about a range as it might get mutated under the hood.
    
    // Design - maybe what we really need are delimited regions like the Reader monad's @local.

    type WRange = Word.Range
    
        

    // Parser is Reader+Error

    type Parser<'a> = Parser of (WRange -> Result<'a>)



    let fail : Parser<'a> = Parser (fun rng -> Fail "fail")

    let apply1 (p : Parser<'a>) (rng : WRange) : Result<'a> = 
        let (Parser pf) = p in pf rng


    let unit (x : 'a) : Parser<'a> = 
        Parser (fun rng -> Okay x)


    let bind (p : Parser<'a>) (f : 'a -> Parser<'b>) : Parser<'b> =
        Parser (fun rng -> 
                    match apply1 p rng with
                    | Okay a -> apply1 (f a) rng
                    | Fail msg -> Fail msg)


    let fmap (f : 'a -> 'b) (p : Parser<'a>) : Parser<'b> = 
        bind p (unit << f)

    type ParserBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = fail


    let parser = new ParserBuilder()

    let empty : Parser<'a> = fail


    let delimit (upd : WRange -> WRange) (p : Parser<'a>) : Parser<'a> = 
        Parser (fun rng ->
                    let rng1 = rng.Duplicate
                    apply1 p (upd rng1))


    // Left-biased
    let alt (p : Parser<'a>) (q : Parser<'a>) : Parser<'a> =
        Parser (fun rng -> 
                    match apply1 p rng with
                    | Okay a -> Okay a
                    | Fail _ -> apply1 q rng)



    let (<|>) (p : Parser<'a>) (q : Parser<'a>) : Parser<'a> = alt p q


    let ap (p : Parser<'a -> 'b>) (q : Parser<'a>) : Parser<'b> =
        parser { let! f = p
                 let! a = q
                 return (f a)
               }

    let (<*>) (p : Parser<'a -> 'b>) (q : Parser<'a>) : Parser<'b> = ap p q

    let apLeft (p : Parser<'a>) (q : Parser<'b>) : Parser<'a> = 
        parser { let! a = p
                 let! _ = q
                 return a
               }

    let apRight (p : Parser<'a>) (q : Parser<'b>) : Parser<'b> = 
        parser { let! _ = p
                 let! a = q
                 return a
               }

    let ( *> ) (p : Parser<'a>) (q : Parser<'b>) : Parser<'b> = apRight p q
    let ( <* ) (p : Parser<'a>) (q : Parser<'b>) : Parser<'a> = apLeft p q


    let text : Parser<string> = 
        Parser (fun rng -> 
                    match rng with
                    | null -> Fail "Range is null"
                    | rng1 -> Okay <| rng1.Text)


    // Note - the index is local within the range
    // Also indexing is from 1 (must check this...)
    let withTable (i:int) (p : Parser<'a>) : Parser<'a> =
        Parser (fun rng -> 
                    if i < rng.Tables.Count then 
                        let rng1 = rng.Tables.Item(i).Range
                        apply1 p rng1
                    else Fail "Table out of Range")

    // look for line end...
    let restOfLine : Parser<string> = 
        Parser (fun rng -> 
                    match rng with
                    | null -> Fail "restOfLine - range is null"
                    | rng1 -> Okay <| sRestOfLine rng1.Text)

//
//    // To check - does duplicating range work as expected...
//    let find (s:string) : Parser<unit> = 
//        Parser (fun sk fk doc rng -> 
//                    let mutable rng1 = rng.Duplicate
//                    let ans = rng1.Find.Execute(FindText = rbox s)
//                    Succ ())
//

    let test (p : Parser<'a>) (filepath : string) : 'a = 
        let app = new Word.ApplicationClass (Visible = true) 
        let doc = app.Documents.Open(FileName = rbox filepath)
        let dstart = doc.Content.Start
        let dend = doc.Content.End
        let rng = doc.Range(rbox dstart, rbox dend)
        let ans = apply1 p rng 
        doc.Close(SaveChanges = rbox false)
        app.Quit()
        match ans with
        | Fail msg -> failwith msg
        | Okay a -> a


