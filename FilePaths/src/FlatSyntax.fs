[<AutoOpen>]
module FilePath.FlatSyntax

open FilePath.Syntax
open FParsec


type Element = 
    | Dir of Name * Mode * TimeStamp * Element list
    | File of Name * Mode * TimeStamp * FileLength


type Block = Block of Name * Element list

type Listing = Block list


// Parsing output of "dir -Recurse"

// Utility combinators
let ws : Parser<string,unit> = manyChars (pchar ' ' <|> pchar '\t')
let ws1 : Parser<string,unit> = many1Chars (pchar ' ' <|> pchar '\t')

let symbol p = p .>> ws
let symbol1 p = p .>> ws1

let lineOf (p:Parser<'a,unit>) : Parser<'a,unit> = 
    p .>> newline

let twice (p:Parser<'a,unit>) : Parser<('a * 'a),unit> = pipe2 p p (fun a b -> (a,b))

let blankline : Parser<unit,unit> = lineOf ws >>. preturn ()

// Syntax specific parsers

// Files names continue to end-of-line (this accommodates spaces in file names)

let pName : Parser<Name,unit> = restOfLine false |>> (fun s -> s.TrimEnd ())

// Note this is UK centric    
let pDate : Parser<Date,unit> = 
    pipe3 pint32 (pchar '/' >>. pint32) (pchar '/' >>. pint32) (fun d m y -> { year=y; month=m; day=d })
    
let pTime : Parser<Time,unit> = 
    pipe2 pint32 (pchar ':' >>. pint32) (fun h m -> { hour=h; minute=m })

// Note this is UK centric
let pTimeStamp : Parser<TimeStamp,unit> = 
    pipe2 (symbol1 pDate) pTime (fun d t -> TimeStamp(d,t))


let pMode : Parser<Mode,unit> = many1Chars (lower <|> pchar '-') 

let isDir (s:Mode) : bool = s.StartsWith("d")


// Note - if directory name longer than 100(?) chars it is listed on a new line
let pDirectoryName : Parser<Name,unit> = 
    manyChars (pchar ' ') >>. pstring "Directory:" >>. spaces >>. pName

let sepStrings (strs : string list) : Parser<string list,unit> = 
    let rec fn xs = match xs with
                    | [] -> preturn []
                    | [s] -> pstring s |>> List.singleton
                    | s :: ss -> pipe2 (symbol1 (pstring s)) (fn ss) (fun y ys -> List.Cons(y,ys))
    fn strs

// Mode                LastWriteTime         Length Name 
let pTitles : Parser<unit,unit> = 
    sepStrings ["Mode"; "LastWriteTime"; "Length"; "Name"] >>. ws >>. newline >>. restOfLine false >>. preturn ()
    


// Note - files system is flat at parse time (needs postprocessing to build)
let pElement : Parser<Element,unit> = 
    let parseK mode = 
        if isDir mode then pipe2 (symbol1 pTimeStamp) pName (fun t s -> Dir (s,mode,t,[]))
        else pipe3 (symbol1 pTimeStamp) (symbol1 pint64) pName (fun t l s -> File(s,mode,t,l))
    (symbol pMode) >>= parseK


let pBlock : Parser<Block, unit> = 
    pipe3 (spaces >>. lineOf pDirectoryName) 
          (twice blankline >>. lineOf pTitles >>. many1 (lineOf pElement))
          spaces
          (fun s es z -> Block(s,es))

let pListing : Parser<Listing,unit> = many (pBlock .>> spaces)