[<AutoOpen>]
module ExtractorCombinators

// Add a reference via the COM tab 
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop

[<AutoOpen>]
module Parsers = 

    let rbox (v : 'a) : obj ref = ref (box v)

    type WDoc = Word.Document

    // TODO - what object is best to store as a cursor - a range or an integer position?
    // We should be very careful about a range as it might get mutated under the hood.
    type WRange = Word.Range
    
    type FailK<'r> = WRange -> 'r

    type SuccK<'a,'r> = 'a -> FailK<'r> -> WDoc -> WRange -> 'r
        

    // Note from a Range we could get back to Doc

    type Parser<'a,'r> = Parser of (SuccK<'a,'r> -> FailK<'r> -> WDoc -> WRange -> 'r)

    let apply1 (p : Parser<'a,'r>) (sk : SuccK<'a,'r>) : FailK<'r> -> WDoc -> WRange -> 'r = 
        let (Parser pf) = p in pf sk


    let fail : Parser<'a,'r> = Parser (fun sk fk dc rng -> fk rng)


    let unit (x : 'a) : Parser<'a,'r> = 
        Parser (fun sk fk doc rng -> sk x fk doc rng)


    let bind (p : Parser<'a,'r>) (f : 'a -> Parser<'b,'r>) : Parser<'b,'r> =
        Parser (fun sk -> apply1 p (fun x -> apply1 (f x) sk))

    let fmap (f : 'a -> 'b) (p : Parser<'a,'r>) : Parser<'b,'r> = 
        bind p (unit << f)

    type ParserBuilder() = 
        member self.Return x = unit x
        member self.Bind (p,f) = bind p f
        member self.Zero () = fail


    let parser = new ParserBuilder()

    let empty : Parser<'a,'r> = fail

    // Use "find" to get the first locus

    let private furthest (x : WRange) (y : WRange) : WRange = if x.Start >= y.Start then x else y

    let alt (p : Parser<'a,'r>) (q : Parser<'a,'r>) : Parser<'a,'r> =
        Parser (fun sk fk ts n -> 
                    let fkp n1 =
                        let fkq n2 = fk (furthest n1 n2)
                        apply1 q sk fkq ts n
                    apply1 p sk fkp ts n)

    let (<|>) (p : Parser<'a,'r>) (q : Parser<'a,'r>) : Parser<'a,'r> = alt p q


    let ap (p : Parser<'a -> 'b,'r>) (q : Parser<'a,'r>) : Parser<'b,'r> =
        parser { let! f = p
                 let! a = q
                 return (f a)
               }

    let (<*>) (p : Parser<'a -> 'b,'r>) (q : Parser<'a,'r>) : Parser<'b,'r> = ap p q

    let apLeft (p : Parser<'a,'r>) (q : Parser<'b,'r>) : Parser<'a,'r> = 
        parser { let! a = p
                 let! _ = q
                 return a
               }

    let apRight (p : Parser<'a,'r>) (q : Parser<'b,'r>) : Parser<'b,'r> = 
        parser { let! _ = p
                 let! a = q
                 return a
               }

    let ( *> ) (p : Parser<'a,'r>) (q : Parser<'b,'r>) : Parser<'b,'r> = apRight p q
    let ( <* ) (p : Parser<'a,'r>) (q : Parser<'b,'r>) : Parser<'a,'r> = apLeft p q

    let updRangeToEnd (rng : Word.Range) : unit = 
        let end1 = rng.End
        rng.Start <- end1


    let Text : Parser<string,'r> = 
        Parser (fun sk fk doc rng -> 
                    match rng with
                    | null -> fk rng
                    | rng1 -> let txt = rng1.Text
                              let _ = updRangeToEnd rng1
                              sk txt fk doc rng1)

    let ToTable (i:int) : Parser<unit,'r> =
        Parser (fun sk fk doc rng -> 
                    if i < doc.Tables.Count then 
                        let rng1 = doc.Tables.Item(1).Range
                        sk () fk doc rng1
                    else fk rng)

                    
     


    let test (p : Parser<'a,'r>) (filepath : string) : 'a = 
        let app = new Word.ApplicationClass (Visible = true) 
        let doc = app.Documents.Open(FileName = rbox filepath)
        let dstart = doc.Content.Start
        let dend = doc.Content.End
        let rng = doc.Range(rbox dstart, rbox dend)
        let sk = fun x fk ts n -> x
        let fk = fun n -> failwith "Add error handling..."
        let ans = apply1 p sk fk doc rng 
        doc.Close(SaveChanges = rbox false)
        app.Quit()
        ans

