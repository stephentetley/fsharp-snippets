[<AutoOpen>]
module ExtractorCombinators

// Add a reference via the COM tab 
// All that PIA stuff is outdated for Office 365 / .Net 4.5 / VS2015 
open Microsoft.Office.Interop

[<AutoOpen>]
module Parsers = 

    let rbox (v : 'a) : obj ref = ref (box v)

    type WDoc = Word.Document

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

