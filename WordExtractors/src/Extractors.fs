[<AutoOpen>]
module Extractors

    // Add references via the COM tab for Office and Word
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
    
        

    // Extractor is Reader+Error

    type Extractor<'a> = Extractor of (WRange -> Result<'a>)



    let fail : Extractor<'a> = Extractor (fun rng -> Fail "fail")

    let apply1 (p : Extractor<'a>) (rng : WRange) : Result<'a> = 
        let (Extractor pf) = p in pf rng


    let unit (x : 'a) : Extractor<'a> = 
        Extractor (fun rng -> Okay x)


    let bind (p : Extractor<'a>) (f : 'a -> Extractor<'b>) : Extractor<'b> =
        Extractor <| fun rng -> 
            match apply1 p rng with
            | Okay a -> apply1 (f a) rng
            | Fail msg -> Fail msg


    let fmap (f : 'a -> 'b) (p : Extractor<'a>) : Extractor<'b> = 
        bind p (unit << f)

    type ExtractorBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = fail


    let extractor = new ExtractorBuilder()

    let empty : Extractor<'a> = fail


    let delimit (upd : WRange -> WRange) (p : Extractor<'a>) : Extractor<'a> = 
        Extractor <| fun rng ->
            let rng1 = rng.Duplicate
            apply1 p (upd rng1)


    // Left-biased
    let alt (p : Extractor<'a>) (q : Extractor<'a>) : Extractor<'a> =
        Extractor <| fun rng -> 
            match apply1 p rng with
            | Okay a -> Okay a
            | Fail _ -> apply1 q rng



    let (<|>) (p : Extractor<'a>) (q : Extractor<'a>) : Extractor<'a> = alt p q


    let ap (p : Extractor<'a -> 'b>) (q : Extractor<'a>) : Extractor<'b> =
        extractor { let! f = p
                    let! a = q
                    return (f a)
                  }

    let (<*>) (p : Extractor<'a -> 'b>) (q : Extractor<'a>) : Extractor<'b> = ap p q

    let apLeft (p : Extractor<'a>) (q : Extractor<'b>) : Extractor<'a> = 
        extractor { let! a = p
                    let! _ = q
                    return a
                  }

    let apRight (p : Extractor<'a>) (q : Extractor<'b>) : Extractor<'b> = 
        extractor { let! _ = p
                    let! a = q
                    return a
                  }

    let ( *> ) (p : Extractor<'a>) (q : Extractor<'b>) : Extractor<'b> = apRight p q
    let ( <* ) (p : Extractor<'a>) (q : Extractor<'b>) : Extractor<'a> = apLeft p q


    let text : Extractor<string> = 
        Extractor <| fun rng -> 
            match rng with
            | null -> Fail "Range is null"
            | rng1 -> Okay <| rng1.Text


    // Note - the index is local within the range
    // Also indexing is from 1 (must check this...)
    let withTable (i:int) (p : Extractor<'a>) : Extractor<'a> =
        Extractor <| fun rng -> 
            if i < rng.Tables.Count then 
                let rng1 = rng.Tables.Item(i).Range
                apply1 p rng1
            else Fail "Table out of Range"

    // look for line end...
    let restOfLine : Extractor<string> = 
        Extractor <| fun rng -> 
            match rng with
            | null -> Fail "restOfLine - range is null"
            | rng1 -> Okay <| sRestOfLine rng1.Text


    // To check - does duplicating range work as expected...
    let find (s:string) : Extractor<string> = 
        let upd (rng : WRange) = 
            rng.Find.ClearFormatting ()
            let ans = rng.Find.Execute(FindText = rbox s)
            rng
        delimit upd text


    let test (p : Extractor<'a>) (filepath : string) : 'a = 
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


