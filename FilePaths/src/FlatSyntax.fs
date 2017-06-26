[<RequireQualifiedAccess>]
module FilePath.FlatSyntax

open System
open FParsec
open FSharpx.Collections
open FilePath.Syntax

// Note - a flat representation might be more useful than just an interim form close to input syntax.


type File1 = File1 of Name * Mode * TimeStamp * FileLength

type Element = 
    | Dir of Name * Mode * TimeStamp * Element list
    | File of File1


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
    pipe3 pint32 (pchar '/' >>. pint32) (pchar '/' >>. pint32) (fun d m y -> { Year=y; Month=m; Day=d })
    
let pTime : Parser<Time,unit> = 
    pipe2 pint32 (pchar ':' >>. pint32) (fun h m -> { Hour=h; Minute=m })

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
let pHeadings : Parser<unit,unit> = 
    sepStrings ["Mode"; "LastWriteTime"; "Length"; "Name"] >>. ws >>. newline >>. restOfLine false >>. preturn ()
    


// Note - files system is flat at parse time (needs postprocessing to build)
let pElement : Parser<Element,unit> = 
    let parseK mode = 
        if isDir mode then pipe2 (symbol1 pTimeStamp) pName (fun t s -> Dir (s,mode,t,[]))
        else pipe3 (symbol1 pTimeStamp) (symbol1 pint64) pName (fun t l s -> File(File1(s,mode,t,l)))
    (symbol pMode) >>= parseK


let pBlock : Parser<Block, unit> = 
    pipe3 (spaces >>. lineOf pDirectoryName) 
          (twice blankline >>. lineOf pHeadings >>. many1 (lineOf pElement))
          spaces
          (fun s es z -> Block(s,es))

let pListing : Parser<Listing,unit> = many (pBlock .>> spaces)


let readListing (path:string) : Choice<string,Listing> = 
    let ans1 = runParserOnFile pListing () path Text.ASCIIEncoding.ASCII
    match ans1 with
    | Success(a,_,_) -> Choice2Of2 a
    | Failure(s,_,_) -> Choice1Of2 s


//// Conversion



let getFiles (xs: Element list) : File1 list = 
    let step ac e =
        match e with
        | Dir(_) -> ac
        | File(x) -> x::ac        
    List.fold step [] xs

let getRoot (x:Listing) : Block option = 
    match x with
    | x::xs -> Some(x)
    | _ -> None

let getDescendants (xs:Listing) : Map<Name, Element list> = 
    let step ac e = match e with | Block(s,ys) -> Map.add s ys ac
    List.fold step Map.empty xs
    


let file1 (x:File1) : File = 
    match x with
    | File1(s,m,t,l) -> { Name=s; Mode=m; TimeStamp=t; Length=l }

// element1 does not fill out subdirectories
let element1 (x:Element) : Choice<Directory,File> = 
    match x with
    | File (x) -> Choice2Of2 <| file1 x
    | Dir (s,m,t,xs) -> Choice1Of2 { Name=s
                                   ; Mode=m
                                   ; TimeStamp=t
                                   ; SubDirs=[]
                                   ; Files=List.map file1 <| getFiles xs }

let rec children (s:Name) (m:Map<Name, Element list>) : Directory list =
    let allkids = Map.tryFind s m |> (fun x -> match x with 
                                                | None -> []
                                                | Some(xs) -> xs)
    let allkids2 = List.map element1 allkids
    let mkLongname suffix = s + "\\" + suffix
    let buildup (x:Directory)  = 
        let longname = mkLongname x.Name
        let kids1 = children longname m
        { x with SubDirs=kids1; Name=longname}
    List.map buildup <| List.choice1s allkids2
    


let topdown (x:Listing) : Root option = 
    let optroot = getRoot x
    let mkids = getDescendants x
    match optroot with 
    | None -> None
    | Some(Block(name,elems)) -> let kids = children name mkids
                                 Some <| { Name=name
                                         ; Files=List.map file1 <| getFiles elems
                                         ; SubDirs=kids }

